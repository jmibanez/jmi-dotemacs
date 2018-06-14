;;; projectile.init.el -- Projectile config

;;; Commentary:
;;; This actually also loads the textmate package; we use textmate's
;;; mode map to bind Cmd-T (super-t)

;;; Code:

(use-package textmate
  :config
  (textmate-mode)
  (define-key *textmate-mode-map* [(super /)] 'comment-or-uncomment-region-or-line))

(use-package projectile
  :config
  (projectile-mode)

  (defun projectile-bind-cmd-t ()
    (define-key *textmate-mode-map* [(super t)] 'projectile-find-file))
  (add-hook 'textmate-mode-hook 'projectile-bind-cmd-t)
  (global-set-key [(super o)] 'projectile-switch-project))

(use-package helm-projectile
  :config
  (helm-projectile-toggle 0))

;;; projectile.init.el ends here
