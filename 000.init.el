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

;; If not on macOS, turn off the menu bar
(if (not (eq system-type 'darwin))
    (menu-bar-mode -1))
(scroll-bar-mode -1)

;; User Details
(setq user-full-name "Jan Michael Ibanez")
(setq user-mail-address "jm@jmibanez.com")

;; exec-path
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(push "/usr/local/bin" exec-path)

;; exec-path for pyenv, rbenv
(let ((rbenv-shim-path (concat (getenv "HOME") "/.rbenv/shims"))
      (pyenv-shim-path (concat (getenv "HOME") "/.pyenv/shims")))
  (setenv "PATH" (concat rbenv-shim-path ":" pyenv-shim-path (getenv "PATH")))
  (push rbenv-shim-path exec-path)
  (push pyenv-shim-path exec-path))

;; Suppress default initial buffer
(setq initial-buffer-choice t)

;;; 000.init.el ends here
