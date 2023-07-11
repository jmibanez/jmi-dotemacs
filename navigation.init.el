;;; navigation.init.el -- Navigation and movement in Emacs

;;; Commentary:
;;; This configures a bunch of stuff I use for getting around my Emacs
;;; instance, including windmove (using a custom set of bindings) and
;;; projectile.  Also included is a bare helm config here, which I'll
;;; populate later on as I get used to using helm

;;; Code:

;; display-buffer tweaks
(setq switch-to-buffer-obey-display-actions t)
(setq switch-to-buffer-in-dedicated-window 'pop)

(defun mp/make-display-buffer-matcher-function (major-modes)
  ;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
  (lambda (buffer-name action)
    (with-current-buffer buffer-name (apply #'derived-mode-p major-modes))))


(setq display-buffer-alist
      '(("\\*vterm\\*"
         (display-buffer-in-side-window)
         (side . bottom)
         (slot . 0)
         (window-parameters . ((no-delete-other-windows . t)))
         (mode vterm-mode vterm-copy-mode))))
(defun mp/toggle-window-dedication ()
  "Toggle window dedication in the selected window."
  ;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
  (interactive)
  (set-window-dedicated-p (selected-window)
     (not (window-dedicated-p (selected-window)))))


;; Projectile
(use-package projectile
  :init
  (setq projectile-completion-system 'auto)

  :config
  (projectile-mode)

  :after vertico)

;; Savehist, persist history (so Vertico works better)
(use-package savehist
  :config
  (savehist-mode))

;; Vertico
(use-package vertico
  :config
  (setq vertico-count 25)
  (setq vertico-resize t)

  (vertico-mode)

  :bind
  (("s-t"    . projectile-find-file)
   ("s-o"    . projectile-switch-project)))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package posframe)
(use-package vertico-posframe
  :config
  (vertico-posframe-mode))

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
  :ensure nil ;; System package
  :bind (("<f7>"         .  bookmark-jump)
         ("S-<f7>"       .  bookmark-set)))

;; Fullscreen
(use-package frame
  :ensure nil ;; System package
  :bind (("s-<return>"   .  toggle-frame-fullscreen)
         ("C-s-<return>" .  toggle-frame-maximized)))

;; Focus mode --
(use-package focus
  :config
  (add-to-list 'focus-mode-to-thing '(c-mode     .  lsp-folding-range))
  (add-to-list 'focus-mode-to-thing '(java-mode  .  defun))

  :after (lsp lsp-mode lsp-java))

;; Buffer Move: I actually want to swap buffers around windows sometimes, so this is useful...
(use-package buffer-move
  :bind
  (([C-S-s-up]      . buf-move-up)
   ([C-S-s-down]    . buf-move-down)
   ([C-S-s-left]    . buf-move-left)
   ([C-S-s-right]   . buf-move-right)))

;;; navigation.init.el ends here
