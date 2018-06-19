;;; 001-theme.init.el -- Theming-specific functions and config

;;; Commentary:
;;; This configures the modeline (via powerline, spaceline, and mode
;;; icons) and adds a few functions to toggle the Solarized Emacs
;;; theme between light and dark

;;; Code:

;; Set default fonts
(set-frame-font "Hack 10" nil t)

;; Load theme
(use-package color-theme-solarized
  :ensure t

  :init
  (setq frame-background-mode 'dark)

  :config
  (load-theme 'solarized t)

  ;; Functions to toggle between light and dark
  (defun jmi/apply-solarized-dark ()
    (interactive)
    (set-frame-parameter nil 'background-mode 'dark)
    (enable-theme 'solarized))

  (defun jmi/apply-solarized-light ()
    (interactive)
    (set-frame-parameter nil 'background-mode 'light)
    (enable-theme 'solarized)))


;; Modeline config
(use-package powerline
  :ensure t
  :init
  (setq powerline-default-separator 'curve))

(use-package spaceline-config
  :ensure spaceline
  :init
  (setq spaceline-helm-mode t)

  :config
  (spaceline-spacemacs-theme)

  :after helm)

(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode))

;; Pop-up windows when display-buffer
(setq pop-up-windows t)

;; Indicate buffer boundaries
(setq-default indicate-buffer-boundaries '((top . left) (t . right)))


;;; 001-theme.init.el ends here
