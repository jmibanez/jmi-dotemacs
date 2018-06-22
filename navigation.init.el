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
  ;; Fuzzy matches everywhere!
  (setq helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-M-x-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-buffers-fuzzy-matching t)

  ;; Adopted from sachac's config
  ;; http://pages.sachachua.com/.emacs.d/Sacha.html
  (setq helm-ff-skip-boring-files t)

  ;; Use the same buffer as where invoked
  (setq helm-split-window-default-side 'same
        helm-split-window-inside-p nil
        helm-reuse-last-window-split-state nil)

  :config
  (helm-mode 1)

  ;; Advise helm-show-kill-ring so we split verfically
  (defun jmi/advice-helm-split-existing-window (orig-fn &rest args)
    (let ((helm-split-window-default-side 'below)
          (helm-split-window-inside-p t))
      (apply orig-fn args)))

  (advice-add 'helm-show-kill-ring :around
              #'jmi/advice-helm-split-existing-window)

  :bind
  (("C-c h"   .  helm-mini)

   ;; DEEP END STUFF: This enables helm for C-x C-f
   ("C-x C-f" .  helm-find-files)
   ;; Also bind M-x
   ("M-x"     .  helm-M-x)
   ;; ... and apropos
   ("C-h a"   .  helm-apropos)
   ;; ... and switch-to-buffer
   ("C-x b"   .  helm-buffers-list)

   ;; Huh. helm-show-kill-ring looks good as an alternative to
   ;; yank-pop -- makes it easier to see exactly what I want to get
   ;; from the kill ring...
   ("M-y"     .  helm-show-kill-ring))

  :demand)

;; Better Helm fuzzy matches
(use-package helm-fuzzier
  :config
  (helm-fuzzier-mode 1)

  :after (helm helm-flx))
(use-package helm-flx
  :after helm)

;; helm-swoop -- for navigating quickly through matches in a buffer
(use-package helm-swoop
  :config
  (setq helm-swoop-use-fuzzy-match t)
  ;; Weird that helm-swoop uses this var as helm-display-function,
  ;; *not* as helm-split-window-preferred-function; anyway, I want
  ;; similar behavior as helm-show-kill-ring, so use
  ;; helm-display-function
  (setq helm-swoop-split-window-function helm-display-function)

  ;; ... we then advice helm-swoop instead with our advice-fn
  (advice-add 'helm-swoop :around
              #'jmi/advice-helm-split-existing-window)

  :bind (("C-s-s"  .  helm-swoop))

  :after (helm helm-fuzzier))

;; Projectile
(use-package projectile
  :init
  (setq projectile-completion-system 'helm)

  :config
  (projectile-mode)

  :after helm)

(use-package helm-projectile
  :config
  (helm-projectile-on)

  :bind
  (("s-t"   .  helm-projectile)
   ("s-o"   .  projectile-switch-project))

  :after (projectile helm))

;; Window move via cmd-ctrl-(vim keys)
;; Changed to be consistent with how we've bound things in our iTerm2 config
(global-set-key (kbd "C-s-h") 'windmove-left)
(global-set-key (kbd "C-s-l") 'windmove-right)
(global-set-key (kbd "C-s-k") 'windmove-up)
(global-set-key (kbd "C-s-j") 'windmove-down)

;; Use Emacs session management
(use-package session
  :config
  (setq session-use-package t)
  (session-initialize)
  (add-to-list 'session-globals-exclude 'org-mark-ring))

(use-package fill-column-indicator
  :init
  ;; Change the color for the indicator to match Solarized Dark
  (setq fci-rule-color "#888888")

  :config
  (define-globalized-minor-mode jmi-global-fci-mode
    fci-mode
    turn-on-fci-mode))


;; Bind F7/S-F7 to bookmark
(use-package bookmark
  :bind (("<f7>"         .  bookmark-jump)
         ("S-<f7>"       .  bookmark-set)))

;; Fullscreen
(use-package frame
  :bind (("s-<return>"   .  toggle-frame-fullscreen)
         ("C-s-<return>" .  toggle-frame-maximized)))

;;; navigation.init.el ends here
