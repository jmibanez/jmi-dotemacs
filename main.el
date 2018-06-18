;;; main.el -- Entry point for all my start files

;;; Commentary:
;;; This drives loading of all .init.el files in ~/.emacs.init

;;; Code:

;; Basic init funs
(defconst jmi/my-emacs-init-path (file-name-directory load-file-name)
  "Directory where all my init files live.")

(defmacro jmi/dotemacs-do-module (filename)
  "Load the specified FILENAME from the init directory."
  (load-file (concat jmi/my-emacs-init-path filename)))


(defun jmi/list-init-files (directory)
  "List all .init.el files inside DIRECTORY."
  (if (not (file-exists-p directory))
      '()
    (let (init-files-list
          (current-dir-list (directory-files-and-attributes directory t)))
      (dolist (dir-item current-dir-list init-files-list)
        (if (equal ".init.el" (substring (car dir-item) -8))
            (setq init-files-list
                  (cons (car dir-item)
                        init-files-list)))))))

;; Packages
(package-initialize)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Bootstrap: Ensure use-package is installed
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Load all init modules
(mapc 'load-file
      (sort (jmi/list-init-files jmi/my-emacs-init-path)
            'string-lessp))

;; If there are any customizations per-machine, per-user, load them as
;; well
(mapc 'load-file
      (sort (jmi/list-init-files (concat jmi/my-emacs-init-path
                                         (symbol-name system-type)))
            'string-lessp))

;; Load keybindings
(jmi/dotemacs-do-module "keybindings.el")

;;; main.el ends here

