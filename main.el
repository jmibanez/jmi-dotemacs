;;; main.el -- Entry point for all my start files

;;; Commentary:
;;; This drives loading of all .init.el files in ~/.emacs.init

;;; Code:

;; Basic init funs
(defconst jmi/my-emacs-init-path (file-name-directory load-file-name)
  "Directory where all my init files live.")

(defmacro jmi/dotemacs-do-module (filename)
  "Load the specified FILENAME from the init directory."
  `(load-file ,(concat jmi/my-emacs-init-path filename)))


(defun jmi/list-init-files (directory)
  "List all .init.el files inside DIRECTORY."
  (if (not (file-exists-p directory))
      '()
    (let (init-files-list
          (current-dir-list (directory-files-and-attributes directory t)))
      (dolist (dir-item current-dir-list init-files-list)
        (if (equal ".init.el" (substring (car dir-item) -8))
            (let ((dir-item-base (substring (car dir-item) 0 -3)))
                (setq init-files-list
                      (cons dir-item-base
                            init-files-list))))))))

;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-activate-all)

;; Bootstrap: Ensure bootstrap packages are installed
(defvar jmi/bootstrap-packages '(s use-package use-package-ensure-system-package session system-packages)
  "Packages that should be installed as early as possible.")

(defun jmi/bootstrap-packages-installed-p ()
  "Check whether bootstrap packages are installed."
  (seq-reduce
   (lambda (a b) (and a b))
   (mapcar (lambda (pkg) (package-installed-p pkg))
	   jmi/bootstrap-packages)
   t))

(unless (jmi/bootstrap-packages-installed-p)
  (message "%s" "Installing bootstrap packages...")
  (package-refresh-contents)

  (dolist (pkg jmi/bootstrap-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))


(require 'use-package)
(require 'system-packages)
(require 'use-package-ensure-system-package)
(require 's)

(defun jmi/platform-init-path ()
  "Return path to directory containing platform-specific init files."
  (let* ((platform-dir (symbol-name system-type))
         (sanitized-platform-dir
          (if (s-contains? "/" platform-dir)
              (car (last (s-split "/" platform-dir)))
            platform-dir)))
    (concat jmi/my-emacs-init-path
            sanitized-platform-dir)))

;; If there are any customizations per-machine, per-user, load them
;; first, so that our config can refer to them
(mapc 'load
      (sort (jmi/list-init-files (jmi/platform-init-path))
            'string-lessp))

;; Load all init modules
(mapc 'load
      (sort (jmi/list-init-files jmi/my-emacs-init-path)
            'string-lessp))


;;; main.el ends here

