;;; 000.init.el -- Early initialization
;;;

;;; Commentary:
;;; This is copied over to early-init.el

;;; Code:

;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-activate-all)

;; Bootstrap: Ensure bootstrap packages are installed
(defvar jmi/bootstrap-packages '(s session system-packages)
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

;; Start server
(server-start)

;; As early as possible, disable menu, scroll, and tool bar
(tool-bar-mode -1)

;; If not on macOS (or in a terminal), turn off the menu bar
(when (not (and (eq system-type 'darwin)
                (display-graphic-p)))
  (menu-bar-mode -1))

;; Set default frame size
(setq default-frame-alist '((width . 120)
                            (height . 70)
                            (vertical-scroll-bars . nil)))

;; Graphic terminals: Don't display scroll bar
(scroll-bar-mode -1)

;; On macOS, turn off titlebars
(when (and (eq system-type 'darwin))
  (add-to-list 'default-frame-alist '(undecorated . t)))

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

;; Enable magic GC hack
(use-package gcmh
  :ensure t
  :demand t
  :config (gcmh-mode 1))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;; 000.init.el ends here
