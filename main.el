;;; main.el -- Entry point for all my start files

;;; Commentary:
;;; This drives loading of all .init.el files in ~/.emacs.init

;;; Code:

;; Basic init funs
(defconst jmi/my-emacs-init-path (file-name-directory load-file-name)
  "Directory where all my init files live.")

(defconst jmi/early-init-file (locate-user-emacs-file "early-init.el")
  "Location of early-init file.")

(defconst jmi/early-init-file-source (concat jmi/my-emacs-init-path "000.init.el")
  "Location of early-init source file.")

(defmacro jmi/dotemacs-do-module (filename)
  "Load the specified FILENAME from the init directory."
  `(load-file ,(concat jmi/my-emacs-init-path filename)))

;; Add pkg for init-only defined packages that we can leverage use-package on
(add-to-list 'load-path (concat jmi/my-emacs-init-path "/pkg"))

(setq custom-file (concat jmi/my-emacs-init-path "001-custom.init.el"))

;; Ensure that 000.init.el is copied to early-init.el; eval if it's
;; newer or non-existent
(defun jmi/update-early-init ()
  "Update early-init.el and byte-compile it"
  (interactive)
  (copy-file jmi/early-init-file-source jmi/early-init-file t)
  (byte-compile-file jmi/early-init-file))

(unless (not (file-newer-than-file-p jmi/early-init-file-source
                                     jmi/early-init-file))
  (copy-file jmi/early-init-file-source jmi/early-init-file t)
  (load jmi/early-init-file-source nil nil :no-suffix))


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


;; If we're debugging init, add more logging
(when init-file-debug
  (setopt use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t))


(defun jmi/platform-init-path ()
  "Return path to directory containing platform-specific init files."
  (let* ((platform-dir (symbol-name system-type))
         (sanitized-platform-dir
          (if (s-contains? "/" platform-dir)
              (car (last (s-split "/" platform-dir)))
            platform-dir)))
    (concat jmi/my-emacs-init-path
            sanitized-platform-dir)))


(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(require 'bind-key)

(bind-keys :prefix-map jmi/my-jump-keys-map
           :prefix "<f8>")

;; Also bind C-* to my jump keys
(bind-key "C-*" 'jmi/my-jump-keys-map nil nil)

;; If we aren't running on at least this version of Emacs, error
(if (< emacs-major-version 30)
    (error "This is Emacs version is too old; need to be at least Emacs 30"))

;; If there are any customizations per-machine, per-user, load them
;; first, so that our config can refer to them
(mapc 'load
      (sort (jmi/list-init-files (jmi/platform-init-path))
            'string-lessp))

;; Load all init modules
(mapc 'load
      (seq-remove (lambda (i) (equal "000.init" (substring i -8)))
                  (sort (jmi/list-init-files jmi/my-emacs-init-path)
			'string-lessp)))

(provide 'jmi-dotemacs)

;;; main.el ends here

