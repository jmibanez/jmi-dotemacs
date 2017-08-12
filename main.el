;; Entry point for all my start files

;; Start server
(server-start)
;; (gnuserv-start)

;; As early as possible, disable menu, scroll, and tool bar
(tool-bar-mode -1)

;; If not on macOS, turn off the menu bar
(if (not (eq system-type 'darwin))
    (menu-bar-mode -1))
(scroll-bar-mode -1)

;; Blink the cursor
(blink-cursor-mode 1)

;; Basic init funs
(defmacro jmi/dotemacs-do-module (filename)
  (load-file (concat jmi/my-emacs-init-path filename)))


(defun jmi/list-init-files (directory)
  (let (init-files-list
        (current-dir-list (directory-files-and-attributes directory t)))
    (mapc (lambda (dir-item)
            (if (equal ".init.el" (substring (car dir-item) -8))
                (setq init-files-list
                      (cons (car dir-item)
                            init-files-list))))
          current-dir-list)
    init-files-list))


;; Load paths
(jmi/dotemacs-do-module "loadpaths.el")

;; Color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-charcoal-black-ext)
;; (color-theme-deep-blue)

;; Load all init modules
(mapc 'load-file
      (sort (jmi/list-init-files jmi/my-emacs-init-path)
            'string-lessp))

;; Load keybindings
(jmi/dotemacs-do-module "keybindings.el")

