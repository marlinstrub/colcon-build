;;; colcon.el --- Emacs interface for colcon build tool -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Your Name

;; Author: Your Name <your.email@example.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (transient "0.3.7"))
;; Keywords: tools, convenience, ros, robotics
;; URL: https://github.com/yourusername/colcon.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an Emacs interface for the colcon build tool,
;; commonly used in ROS 2 (Robot Operating System) development.  It
;; allows users to build and test colcon packages directly from Emacs,
;; with features like workspace management, package selection, and
;; a transient interface for command options.

;;; Code:

(defgroup colcon nil
  "Customization group for colcon."
  :group 'tools)

(require 'xml)
(require 'subr-x)  ;; For string-trim

(defvar colcon--package-cache (make-hash-table :test 'equal)
  "Cache for package names, where keys are package.xml file paths and values are package names.")

(defun colcon--get-package-name (file)
  "Get the package name from the package.xml FILE, using cached value if available."
  (or (gethash file colcon--package-cache)
      (let* ((xml (xml-parse-file file))
             (package (car xml))
             (name-node (car (xml-get-children package 'name)))
             (package-name (when name-node
                             (string-trim (car (xml-node-children name-node))))))
        (when package-name
          (puthash file package-name colcon--package-cache))
        package-name)))

(defun colcon-list-packages (workspace-path)
  "List all packages in the WORKSPACE-PATH by finding and parsing package.xml files in the src directory."
  (let* ((src-path (expand-file-name "src" workspace-path))
         (default-directory (expand-file-name "src" workspace-path))
         (cmd "find . -name package.xml")
         (package-files (split-string (shell-command-to-string cmd) "\n" t)))
    (delq nil
          (mapcar #'colcon--get-package-name package-files))))

(defun colcon-select-packages ()
  "Select packages from the current workspace."
  (interactive)
  (if colcon--current-workspace
      (let ((packages (colcon-list-packages (plist-get colcon--current-workspace :path))))
        (setq colcon--selected-packages (completing-read-multiple "Select packages: " packages nil t))
        (unless colcon--selected-packages (user-error "No packages selected")))
    (user-error "No workspace selected.")))

(defcustom colcon-workspaces nil
  "List of known colcon workspaces.
Each element is a plist with keys :name, :path, and optionally :extends."
  :type '(repeat (plist :key-type symbol :value-type string))
  :group 'colcon)

(defvar colcon--current-workspace nil
  "The currently selected workspace.")

(defun colcon-add-workspace (name path)
  "Add a new workspace to =colcon-workspaces'.
NAME is a string to identify the workspace.
PATH is the root directory of the workspace."
  (interactive "sWorkspace name: \nDWorkspace path: ")
  (let* ((tramp-prefix (file-remote-p path))
         (root-path (or tramp-prefix "/"))
         (ros-distros (colcon--get-ros-distros root-path))
         (selected-distro (completing-read "Select ROS distro: " ros-distros nil t))
         (extends (colcon--select-extended-workspaces))
         (workspace (list :name name 
                          :path path 
                          :distro selected-distro
                          :extends extends)))
    (add-to-list 'colcon-workspaces workspace t)
    (message "Added workspace: %s (ROS %s, Extends: %s)" 
             name selected-distro 
             (mapconcat #'identity extends ", "))))

(defun colcon--select-extended-workspaces ()
  "Prompt user to select workspaces that the new workspace extends."
  (let ((available-workspaces (mapcar (lambda (ws) (plist-get ws :name)) colcon-workspaces))
        extended-workspaces)
    (while (and available-workspaces
                (y-or-n-p "Does this workspace extend a workspace? "))
      (let ((selected (completing-read "Select extended workspace: " available-workspaces nil t)))
        (push selected extended-workspaces)
        (setq available-workspaces (delete selected available-workspaces))))
    (nreverse extended-workspaces)))

(defun colcon-remove-workspace (name)
  "Remove a workspace from =colcon-workspaces' by NAME."
  (interactive
   (list (completing-read "Remove workspace: "
                          (mapcar (lambda (ws) (plist-get ws :name)) colcon-workspaces))))
  (setq colcon-workspaces
        (cl-remove-if (lambda (ws) (string= (plist-get ws :name) name))
                      colcon-workspaces))
  (message "Removed workspace: %s" name))

(transient-define-infix colcon--workspace-infix ()
  "Select a workspace from =colcon-workspaces'."
  :class 'transient-lisp-variable
  :variable 'colcon--current-workspace
  :reader (lambda (&rest _)
            (let* ((names (mapcar (lambda (ws) (plist-get ws :name)) colcon-workspaces))
                   (selected (completing-read "Select workspace: " names nil t)))
              (cl-find-if (lambda (ws) (string= (plist-get ws :name) selected))
                                                          colcon-workspaces)))
  :prompt "Select workspace: "
  :description (lambda ()
                 (if colcon--current-workspace
                     (format "Set workspace: %s" (plist-get colcon--current-workspace :name))
                   "Select workspace"))
  :transient t)

(transient-define-infix colcon--packages-infix ()
  "Select a package from the `colcon--current-workspace'."
  :class 'transient-lisp-variable
  :variable 'colcon--selected-packages
  :reader (lambda (&rest _)
            (if colcon--current-workspace
                (let ((packages (colcon-list-packages (plist-get colcon--current-workspace :path))))
                  (completing-read-multiple "Select packages: " packages nil t))
              (user-error "No selected workspace")))
  :prompt "Select packages: "
  :description (lambda ()
                 (if colcon--selected-packages
                     (format "Packages: %s" (mapconcat #'identity colcon--selected-packages ", "))))
  :transient t)

(transient-define-suffix colcon--do-build (&optional args)
  "Execute colcon build with the given ARGS."
  :description "Build"
  :transient nil
  (interactive)
  (if colcon--current-workspace
      (message "Building in workspace: %s" (plist-get colcon--current-workspace :path))
    (user-error "No workspace selected")))

(transient-define-argument colcon--packages-selection-infix ()
  "docs"
  :class 'transient-switches
  :argument-format "--%s"
  :argument-regexp "\\(--\\(packages-select\\|packages-up-to\\|packages-above\\)\\)"
  :choices '("packages-select" "packages-up-to" "packages-above"))

(transient-define-prefix colcon-build ()
  "Transient for colcon build command."
  :value '("--packages-select")
  ["Workspace"
   ("w" "Active workspace" colcon--workspace-infix)]
  ["Packages"
   ("p" "Packages" colcon--packages-infix)
   ("d" "Dependencies" colcon--packages-selection-infix)]
  ["Actions"
   ("b" "Build" colcon--do-build)])

(provide 'colcon)
;;; colcon.el ends here

