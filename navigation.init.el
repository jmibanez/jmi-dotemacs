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
   ([remap find-file]                 .  helm-find-files)
   ;; Also bind M-x
   ([remap execute-extended-command]  .  helm-M-x)
   ;; ... and apropos
   ([remap apropos-command]           .  helm-apropos)
   ;; ... and switch-to-buffer
   ("C-x b"   .  helm-buffers-list)

   ;; Huh. helm-show-kill-ring looks good as an alternative to
   ;; yank-pop -- makes it easier to see exactly what I want to get
   ;; from the kill ring...
   ("M-y"     .  helm-show-kill-ring))

  :demand)

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

  :bind (("C-s-s"  .  helm-swoop)))

;; Projectile
(use-package projectile
  :init
  (setq projectile-completion-system 'helm)

  :config
  (projectile-mode)

  :after helm)

(use-package helm-projectile
  :config
  (setq helm-projectile-truncate-lines t)
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
  :ensure nil ;; System package
  :bind (("s-<return>"   .  toggle-frame-fullscreen)
         ("C-s-<return>" .  toggle-frame-maximized)))


(defmacro eyebrowse-sequence-bindings ()
  (mapcar
   (lambda (i)
     (let ((binding (concat "s-" (number-to-string i)))
           (target (intern (concat "eyebrowse-switch-to-window-config-" (number-to-string i)))))
       `(,binding . ,target)))
   (number-sequence 1 9)))

;; Eyebrowse
(use-package eyebrowse
  :config
  (eyebrowse-mode)

  :bind (("s-1" . eyebrowse-switch-to-window-config-1)
         ("s-2" . eyebrowse-switch-to-window-config-2)
         ("s-3" . eyebrowse-switch-to-window-config-3)
         ("s-4" . eyebrowse-switch-to-window-config-4)
         ("s-5" . eyebrowse-switch-to-window-config-5)
         ("s-6" . eyebrowse-switch-to-window-config-6)
         ("s-7" . eyebrowse-switch-to-window-config-7)
         ("s-8" . eyebrowse-switch-to-window-config-8)
         ("s-9" . eyebrowse-switch-to-window-config-9)
         ("s-0" . eyebrowse-switch-to-window-config-0)))

;;; navigation.init.el ends here
