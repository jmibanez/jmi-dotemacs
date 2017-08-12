(require 'projectile)
(projectile-global-mode)

(require 'helm-projectile)
(helm-projectile-toggle 0)

(defun projectile-bind-cmd-t ()
  (define-key *textmate-mode-map* [(super t)] 'projectile-find-file-dwim))

(add-hook 'textmate-mode-hook 'projectile-bind-cmd-t)
(global-set-key [(super o)] 'projectile-persp-switch-project)

(require 'textmate)
(textmate-mode)


