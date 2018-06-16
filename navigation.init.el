;;; navigation.init.el -- Navigation and movement in Emacs

;;; Commentary:
;;; This configures a bunch of stuff I use for getting around my Emacs
;;; instance, including windmove (using a custom set of bindings) and
;;; projectile.  Also included is a bare helm config here, which I'll
;;; populate later on as I get used to using helm

;;; Code:

;; Helm:
(use-package helm
  :init
  (setq helm-mode-fuzzy-match t)

  ;; Use the same buffer as where invoked
  (setq helm-split-window-default-side 'same)
  (setq helm-reuse-last-window-split-state nil)

  :config
  (helm-mode 1)

  :bind
  (("C-c h"   .  helm-mini)

   ;; DEEP END STUFF: This enables helm for C-x C-f
   ("C-x C-f" .  helm-find-files)
   ;; Also bind M-x
   ("M-x"     .  helm-M-x)
   ;; ... and apropos
   ("C-h a"   .  helm-apropos)))

;; Side-effect: We use a bunch of Textmate-like bindings, so load
;; textmate-mode?
(use-package textmate
  :config
  (textmate-define-comment-line)
  :bind
  (("S-/" . comment-or-uncomment-region-or-line)))

;; Projectile
(use-package projectile
  :config
  (projectile-mode))

(use-package helm-projectile
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)

  :bind
  (("S-t"   .  helm-projectile)
   ("S-o"   .  projectile-switch-project))

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
