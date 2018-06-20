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

  ;; Use the same buffer as where invoked
  (setq helm-split-window-default-side 'same
        helm-reuse-last-window-split-state nil)

  :config
  (helm-mode 1)

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
  :after (helm helm-flx))
(use-package helm-flx
  :after helm)


;; Side-effect: We use a bunch of Textmate-like bindings, so load
;; textmate-mode?
(use-package textmate
  :config
  (textmate-define-comment-line)
  (global-set-key [(super /)] 'comment-or-uncomment-region-or-line))

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
