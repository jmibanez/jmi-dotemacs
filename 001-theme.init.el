;;; 001-theme.init.el -- Theming-specific functions and config

;;; Commentary:
;;; This configures the modeline (via powerline, spaceline, and mode
;;; icons) and adds a few functions to toggle the Solarized Emacs
;;; theme between light and dark

;;; Code:

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
  (setq powerline-default-separator 'curve)

  :config
  (defun jmi/powerline-theme ()
    "Setup my theme (based on the default powerline mode-line)."
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))
                            (separator-left (intern (format "powerline-%s-%s"
                                                            powerline-default-separator
                                                            (car powerline-default-separator-dir))))
                            (separator-right (intern (format "powerline-%s-%s"
                                                             powerline-default-separator
                                                             (cdr powerline-default-separator-dir))))
                            (lhs (list (powerline-raw "%*" nil 'l)
                                       (powerline-buffer-size nil 'l)
                                       (powerline-raw mode-line-mule-info nil 'l)
                                       (powerline-buffer-id nil 'l)
                                       (when (and (boundp 'which-func-mode) which-func-mode)
                                         (powerline-raw which-func-format nil 'l))
                                       (powerline-raw " ")
                                       (funcall separator-left mode-line face1)
                                       (when (boundp 'erc-modified-channels-object)
                                         (powerline-raw erc-modified-channels-object face1 'l))
                                       (powerline-major-mode face1 'l)
                                       (powerline-process face1)
                                       (powerline-minor-modes face1 'l)
                                       (powerline-narrow face1 'l)
                                       (powerline-raw " " face1)
                                       (funcall separator-left face1 face2)
                                       (powerline-vc face2 'r)))
                            (rhs (list (powerline-raw global-mode-string face2 'r)
                                       (funcall separator-right face2 face1)
                                       (powerline-raw "%4l" face1 'l)
                                       (powerline-raw ":" face1 'l)
                                       (powerline-raw "%3c" face1 'r)
                                       (funcall separator-right face1 mode-line)
                                       (powerline-raw " ")
                                       (powerline-raw "%6p" nil 'r)
                                       (powerline-hud face2 face1))))
                       (concat (powerline-render lhs)
                               (powerline-fill face2 (powerline-width rhs))
                               (powerline-render rhs))))))))

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
