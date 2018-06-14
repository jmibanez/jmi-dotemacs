;;; main.el -- Entry point for all my start files

;;; Commentary:
;;; This drives loading of all .init.el files in ~/.emacs.init

;;; Code:

;; Basic init funs
(defconst jmi/my-emacs-init-path (file-name-directory load-file-name))

(defmacro jmi/dotemacs-do-module (filename)
  (load-file (concat jmi/my-emacs-init-path filename)))


(defun jmi/list-init-files (directory)
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
(jmi/dotemacs-do-module "packages.el")

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

