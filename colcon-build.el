;;; colcon-build.el --- Emacs interface for colcon build tool -*- lexical-binding: t; -*-

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

(require 'subr-x)
(require 'tramp)
(require 'transient)
(require 'xml)

(defgroup colcon nil
  "Customization group for colcon."
  :group 'tools)

(defvar colcon--package-cache (make-hash-table :test 'equal)
  "Cache for package names, where keys are package.xml file paths and values are package names.")

(defun colcon--get-ros-distros (path)
  "Get available ROS distributions for the given PATH.
PATH should be the root directory (e.g., / or /docker:dev:/)."
  (let* ((ros-path (expand-file-name "opt/ros/" path))
         (dirs (and (file-directory-p ros-path)
                    (directory-files ros-path nil "^[^.]"))))
    (or dirs
        (error "No ROS distributions found in %s" ros-path))))

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
Each element is a plist with keys :name, :path, :distro, and optionally :extends."
  :type '(repeat (plist :key-type symbol :value-type string))
  :group 'colcon)

(defvar colcon--current-workspace nil
  "The currently selected workspace.")

(defvar colcon--selected-packages nil
  "The selected packages.")

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

(transient-define-infix colcon--cmake-args-infix ()
  "docs"
  :class 'transient-option
  :prompt "CMake arguments: ")

(transient-define-argument colcon--packages-selection-infix ()
  "docs"
  :class 'transient-switches
  :argument-format "--packages-%s"
  :argument-regexp "\\(--packages-\\(select\\|up-to\\|above\\)\\)"
  :choices '("select" "up-to" "above"))

(transient-define-argument colcon--build-type-infix ()
  "docs"
  :class 'transient-switches
  :argument-format "--cmake-args -DCMAKE_BUILD_TYPE=%s"
  :argument-regexp "\\(--cmake-args -DCMAKE_BUILD_TYPE=\\(Release\\|RelWithDebInfo\\|Debug\\)\\)"
  :choices '("Release" "RelWithDebInfo" "Debug"))

(transient-define-argument colcon--build-tests-infix ()
  "docs"
  :class 'transient-switches
  :argument-format "--cmake-args -DGRAVIS_BUILD_TESTS=%s"
  :argument-regexp "\\(--cmake-args -DGRAVIS_BUILD_TESTS=\\(ON\\|OFF\\)\\)"
  :choices '("ON" "OFF"))

(transient-define-argument colcon--console-output-infix ()
  "docs"
  :class 'transient-switches
  :argument-format "--event-handler console_%s"
  :argument-regexp "\\(--event-handler console_\\(direct\\+\\|cohesion\\+\\)\\)"
  :choices '("direct+" "cohesion+"))

(defun colcon--parse-package-selection-infix (args)
  ""
  (let ((select "--packages-select")
        (up-to  "--packages-up-to")
        (above  "--packages-above"))
    (cond ((transient-arg-value select args) select)
          ((transient-arg-value up-to args)  up-to)
          ((transient-arg-value above args)  above)
          (t nil))))

(transient-define-suffix colcon--build-suffix (&optional args)
  "Execute colcon build."
  :description "Build"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (transient-set)
  (let* ((default-directory (plist-get colcon--current-workspace :path))
         (distro-path  (file-name-concat "/opt/ros" (plist-get colcon--current-workspace :distro)))
         (distro-setup (file-name-concat distro-path "setup.bash"))
         (packages     (mapconcat 'identity colcon--selected-packages " "))
         (dependencies (colcon--parse-package-selection-infix args))
         (args-wo-deps (remove dependencies args))
         (colcon-cmd   (concat "source " distro-setup " && "
                               "source " "/home/dev_ws/install/setup.bash" " && "
                               "colcon build " (mapconcat 'identity args-wo-deps " ") " " dependencies " " packages)))
    (if (and colcon--current-workspace colcon--selected-packages)
        (progn
          (message "Command: %s" colcon-cmd)
          ;; (message "Building packages %s in workspace: %s" (mapconcat 'identity colcon--selected-packages ", ") (plist-get colcon--current-workspace :path))
          (compile colcon-cmd))
    (user-error "No active workspace or no selected packages."))))

(defun colcon--clean-packages (packages)
  ""
  (let ((default-directory (plist-get colcon--current-workspace :path)))
    (dolist (package packages)
      (delete-directory (file-name-concat "install" package) t t)
      (delete-directory (file-name-concat "build"   package) t t))))

(transient-define-suffix colcon--clean-suffix ()
  "Clean suffix"
  :description "Clean"
  :transient nil
  (interactive)
  (transient-set)
  (let* ((default-directory (plist-get colcon--current-workspace :path))
         (infix-args   (transient-args transient-current-command))
         (dependencies (colcon--parse-package-selection-infix infix-args)))
    (if (and dependencies (not (string= dependencies "--packages-select")))
        (let* ((selected-packages (mapconcat 'identity colcon--selected-packages " "))
               (colcon-list-cmd (concat "colcon list -n " dependencies " " selected-packages))
               (packages (split-string (shell-command-to-string colcon-list-cmd) "\n")))
          (colcon--clean-packages packages)
          (message "Cleaned packages: %s" (mapconcat 'identity packages " ")))
      (progn
        (colcon--clean-packages colcon--selected-packages)
        (message "Cleaned packages: %s" (mapconcat 'identity colcon--selected-packages " "))))))

;;;###autoload
(transient-define-prefix colcon-build ()
  "Transient for colcon build command."
  :value '("--packages-select" "--event-handler console_direct+" "--cmake-args -DCMAKE_BUILD_TYPE=Release" "--cmake-args -DGRAVIS_BUILD_TESTS=ON")
  ["Workspace"
   ("w" "Active workspace" colcon--workspace-infix)]
  ["Packages"
   ("p" "Packages" colcon--packages-infix)
   ("d" "Dependencies" colcon--packages-selection-infix)]
  ["Build options"
   ("-b" "Build type" colcon--build-type-infix)
   ("-t" "Build tests" colcon--build-tests-infix)
   ;; ("-a" "CMake arguments" "--cmake-args " :class transient-option)
   ("-j" "Threads" "--parallel-workers=" :reader transient-read-number-N+)
   ("-c" "Clean first" "--cmake-clean-first")
   ("-o" "Console output" colcon--console-output-infix)]
  ["Actions"
   [("b" "Build" colcon--build-suffix)]
   [("c" "Clean" colcon--clean-suffix)]])

(provide 'colcon-build)
;;; colcon.el ends here

