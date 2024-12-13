;;; 000.init.el -- Early initialization
;;;

;;; Commentary:
;;; This is loaded first by main.el, and should contain all early
;;; initialization items

;;; Code:

;; Start server
(server-start)

;; As early as possible, disable menu, scroll, and tool bar
(tool-bar-mode -1)

;; If not on macOS (or in a terminal), turn off the menu bar
(when (not (and (eq system-type 'darwin)
                (display-graphic-p)))
  (menu-bar-mode -1))

;; Graphic terminals: Don't display scroll bar
(when (display-graphic-p)
  (scroll-bar-mode -1))

;; On macOS, turn off titlebars
(add-to-list 'default-frame-alist '(undecorated . t))

;; User Details
(setq user-full-name "Jan Michael Ibañez")
(setq user-mail-address "jm@jmibanez.com")

;; exec-path/PATH fixups
;; Remove pyenv/rbenv/nodenv shims, re-add them to the head of the list
(setq exec-path (cl-remove-if (lambda (path-elem)
				(string-match "/shims" path-elem))
                              exec-path))

;; Ensure /usr/local/bin is also in our exec-path
(unless (member "/usr/local/bin" exec-path)
  (push "/usr/local/bin" exec-path))

;; Apply modifications on exec-path to our PATH
(setenv "PATH" (mapconcat 'identity exec-path ":"))

;; Suppress default initial buffer
(setq initial-buffer-choice t)

;; Finally, set up defaults for use-package
(setq use-package-always-ensure t)

;; Add pkg for init-only defined packages that we can leverage use-package on
(add-to-list 'load-path (concat jmi/my-emacs-init-path "/pkg"))

;; Enable magic GC hack
(use-package gcmh
  :ensure t
  :demand t
  :config (gcmh-mode 1))

(setq custom-file (concat jmi/my-emacs-init-path "001-custom.init.el"))
;;; 000.init.el ends here
