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

(defcustom colcon-workspaces nil
  "List of known colcon workspaces.
Each element is a plist with keys :name, :path, :distro, and optionally :extends."
  :type '(repeat (plist :key-type symbol :value-type string))
  :group 'colcon)

(defvar colcon--current-workspace nil
  "The currently selected workspace.")

(defvar colcon--selected-packages nil
  "The selected packages.")

(defvar colcon--package-cache (make-hash-table :test 'equal)
  "Cache for package names, where keys are package.xml file paths and values are package names.")

;;; Workspace management

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

;;; Transient helpers

(defun colcon--generate-source-commands (workspace)
  "Generate source commands for WORKSPACE and its extended workspaces."
  (let* ((distro (plist-get workspace :distro))
         (distro-path (file-name-concat "/opt/ros" distro))
         (distro-setup (file-name-concat distro-path "setup.bash"))
         (extended-workspaces (plist-get workspace :extends))
         (workspace-path (plist-get workspace :path))
         (tramp-prefix (file-remote-p workspace-path))
         (local-workspace-path (if tramp-prefix (file-remote-p workspace-path 'localname) workspace-path))
         (workspace-setup (file-name-concat workspace-path "install/setup.bash"))
         (local-workspace-setup (file-name-concat local-workspace-path "install/setup.bash"))
         (commands (list (concat "source " distro-setup))))
    ;; Add source commands for extended workspaces
    (dolist (ext-ws extended-workspaces)
      (let* ((ext-ws-data (cl-find-if (lambda (ws) (string= (plist-get ws :name) ext-ws)) colcon-workspaces))
             (ext-ws-path (plist-get ext-ws-data :path))
             (local-ext-ws-path (if tramp-prefix (file-remote-p ext-ws-path 'localname) ext-ws-path))
             (ext-ws-setup (file-name-concat ext-ws-path "setup.bash"))
             (ext-ws-install-setup (file-name-concat ext-ws-path "install/setup.bash"))
             (local-ext-ws-setup (file-name-concat local-ext-ws-path "setup.bash"))
             (local-ext-ws-install-setup (file-name-concat local-ext-ws-path "install/setup.bash")))
        (cond
         ((file-exists-p ext-ws-setup)
          (push (concat "source " local-ext-ws-setup) commands))
         ((file-exists-p ext-ws-install-setup)
          (push (concat "source " local-ext-ws-install-setup) commands)))))
    ;; Add source command for the current workspace if it exists
    (when (file-exists-p workspace-setup)
      (push (concat "source " local-workspace-setup) commands))
    ;; Return the commands in reverse order (bottom-up)
    (nreverse commands)))

(defun colcon--clean-packages (packages)
  ""
  (let ((packages-str (mapconcat 'identity packages " ")))
    (if (y-or-n-p (format "Do you want to clean these packages: %s" packages-str))
        (let ((default-directory (plist-get colcon--current-workspace :path)))
          (dolist (package packages)
            (delete-directory (file-name-concat "install" package) t t)
            (delete-directory (file-name-concat "build"   package) t t)
            (message "Cleaned packages: %s" packages-str)))
      (message "Not cleaning any packages."))))

(defun colcon--parse-package-selection-infix (args)
  ""
  (let ((select "--packages-select")
        (up-to  "--packages-up-to")
        (above  "--packages-above"))
    (cond ((transient-arg-value select args) select)
          ((transient-arg-value up-to args)  up-to)
          ((transient-arg-value above args)  above)
          (t nil))))

;;; Transient infixes

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

(transient-define-argument colcon--packages-selection-infix ()
  "docs"
  :class 'transient-switches
  :argument-format "--packages-%s"
  :argument-regexp "\\(--packages-\\(select\\|up-to\\|above\\)\\)"
  :choices '("select" "up-to" "above"))

(transient-define-argument colcon--cmake-preset-infix ()
  "docs"
  :class 'transient-switches
  :argument-format "--cmake-args --preset %s"
  :argument-regexp "\\(--cmake-args --preset \\(default\\)\\)"
  :choices '("default"))

(transient-define-argument colcon--console-output-infix ()
  "docs"
  :class 'transient-switches
  :argument-format "--event-handler console_%s"
  :argument-regexp "\\(--event-handler console_\\(direct\\+\\|cohesion\\+\\)\\)"
  :choices '("direct+" "cohesion+"))

;;; Transient suffixes

(transient-define-suffix colcon--build-suffix (&optional args)
  "Execute colcon build."
  :description "Build"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (transient-set)
  (let* ((default-directory (plist-get colcon--current-workspace :path))
         (source-commands (colcon--generate-source-commands colcon--current-workspace))
         (packages (mapconcat 'identity colcon--selected-packages " "))
         (dependency-selection (colcon--parse-package-selection-infix args))
         (args-wo-deps (remove dependency-selection args))
         (colcon-cmd (concat (mapconcat 'identity source-commands " && ")
                             " && colcon build "
                             (mapconcat 'identity args-wo-deps " ")
                             " " dependency-selection " " packages)))
    (if (and colcon--current-workspace colcon--selected-packages)
        (progn
          (message "Command: %s" colcon-cmd)
          (compile colcon-cmd))
      (user-error "No active workspace or no selected packages."))))

(transient-define-suffix colcon--clean-suffix ()
  "Clean suffix"
  :description "Clean"
  :transient t
  (interactive)
  (transient-set)
  (let* ((default-directory (plist-get colcon--current-workspace :path))
         (infix-args   (transient-args transient-current-command))
         (dependencies (colcon--parse-package-selection-infix infix-args)))
    (if (and dependencies (not (string= dependencies "--packages-select")))
        (let* ((selected-packages (mapconcat 'identity colcon--selected-packages " "))
               (colcon-list-cmd (concat "colcon list -n " dependencies " " selected-packages))
               (packages (split-string (shell-command-to-string colcon-list-cmd) "\n")))
          (colcon--clean-packages packages))
      (colcon--clean-packages colcon--selected-packages))))

;;; Transient prefixes

;;;###autoload
(transient-define-prefix colcon-build ()
  "Transient for colcon build command."
  :value '("--packages-select" "--event-handler console_cohesion+" "--cmake-args --preset default" "--parallel-workers=18")
  ["Workspace"
   ("w" "Active workspace" colcon--workspace-infix)]
  ["Packages"
   ("p" "Packages" colcon--packages-infix)
   ("d" "Dependencies" colcon--packages-selection-infix)]
  ["Build options"
   ("-p" "CMake preset" colcon--cmake-preset-infix)
   ("-c" "Clean cache" "--cmake-clean-cache")
   ("-j" "Threads" "--parallel-workers=" :reader transient-read-number-N+)
   ("-o" "Console output" colcon--console-output-infix)]
  ["Actions"
   [("b" "Build" colcon--build-suffix)]
   [("c" "Clean" colcon--clean-suffix)]])

(provide 'colcon-build)
;;; colcon.el ends here

