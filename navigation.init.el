;;; navigation.init.el -- Navigation and movement in Emacs

;;; Commentary:
;;; This configures a bunch of stuff I use for getting around my Emacs
;;; instance, including windmove (using a custom set of bindings) and
;;; projectile.  Also included is a bare helm config here, which I'll
;;; populate later on as I get used to using helm

;;; Code:

;; Helm:
(use-package helm
  :config
  (global-set-key (kbd "C-c h") 'helm-mini)
  (helm-mode 1)

  ;; DEEP END STUFF: This enables helm for C-x C-f
  (global-set-key (kbd "C-x C-f") #'helm-find-files))

;; Side-effect: We use a bunch of Textmate-like bindings, so load
;; textmate-mode?
(use-package textmate
  :config
  (textmate-define-comment-line)
  (global-set-key [(super /)] 'comment-or-uncomment-region-or-line))

;; Projectile
(use-package projectile
  :config
  (projectile-mode)
  (global-set-key [(super t)] 'helm-projectile)
  (global-set-key [(super o)] 'projectile-switch-project))

(use-package helm-projectile
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)

  :after projectile helm)

;; Window move via cmd-shift-arrow
(global-set-key [(super shift left)]  'windmove-left)
(global-set-key [(super shift right)] 'windmove-right)
(global-set-key [(super shift up)]    'windmove-up)
(global-set-key [(super shift down)]  'windmove-down)

;; Use Emacs session management
(use-package session
  :config
  (session-initialize)
  (add-to-list 'session-globals-exclude 'org-mark-ring))


;;; navigation.init.el ends here