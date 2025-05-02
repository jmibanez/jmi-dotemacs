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
      '(("\\*Flymake diagnostics"
         (display-buffer-in-side-window)
         (side . bottom)
         (slot . 0)
         (window-parameters . ((no-delete-other-windows . t)))
         (mode flymake-diagnostics-buffer-mode))))
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
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-project-root-functions
        '(projectile-root-local
          projectile-root-bottom-up
          projectile-root-marked
          projectile-root-top-down
          projectile-root-top-down-recurring))

  (setq projectile-project-search-path
        '(("~/projects/" . 2)))

  (setq projectile-globally-ignored-files
        '(".classpath"
          ".factorypath"
          ".python-version"
          ".project"))
  (setq projectile-globally-ignored-file-suffixes
        '(".bak"))


  (defun jmi/is-outside-project-dir-p (project-root)
    (not (string-prefix-p (file-truename "~/projects")
                          (file-truename project-root))))

  (defun jmi/is-attic-project-p (project-root)
    (string-prefix-p (file-truename "~/projects/attic")
                     (file-truename project-root)))

  (defun jmi/dotemacs-project-p (project-root)
    (string-prefix-p jmi/my-emacs-init-path
                     (file-truename project-root)))

  (defun jmi/ignored-project-p (project-root)
    (or (jmi/is-attic-project-p project-root)
        (and (jmi/is-outside-project-dir-p project-root)
             (not (jmi/dotemacs-project-p project-root)))))

  (setq projectile-ignored-project-function #'jmi/ignored-project-p)

  (add-to-list 'projectile-globally-ignored-directories
               "\\.settings")

  (projectile-mode)

  :bind
  (("s-o"    . projectile-switch-project)
   ("s-T"    . projectile-toggle-between-implementation-and-test))

  :after (vertico)
  :defer t)

;; Savehist, persist history (so Vertico works better)
(use-package savehist
  :config
  (savehist-mode))

;; Vertico
(use-package vertico
  :config
  (setq vertico-count 25)
  (setq vertico-resize t)

  (vertico-mode))

(use-package counsel)

(use-package consult
  :config
  (setq consult-preview-key nil)

  :bind
  (("M-y"                                  . consult-yank-pop)
   ("C-s-s"                                . consult-line)
   ("s-g"                                  . consult-git-grep)
   ("C-s-!"                                . consult-flymake)
   ([remap goto-line]                      . consult-goto-line)
   ([remap switch-to-buffer]               . consult-buffer)
   ([remap switch-to-buffer-other-window]  . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame]   . consult-buffer-other-frame)))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package popup
  :config
  (setq popup-tip-max-width 100))

(use-package posframe)
(use-package vertico-posframe
  :config

  ;; Shim for latest vertico which renamed vertico--resize-window to vertico--resize
  (cl-defmethod vertico--resize
    (&context ((vertico-posframe-mode-workable-p) (eql t))))

  (vertico-posframe-mode)
  :after posframe)

;; Window move via cmd-ctrl-(vim keys)
;; Changed to be consistent with how we've bound things in our iTerm2 config
(global-set-key (kbd "C-s-h") 'windmove-left)
(global-set-key (kbd "C-s-l") 'windmove-right)
(global-set-key (kbd "C-s-k") 'windmove-up)
(global-set-key (kbd "C-s-j") 'windmove-down)

(use-package framemove
  :load-path "~/elisp/framemove"

  :init
  (setq framemove-hook-into-windmove t))


;; Use Emacs session management
(use-package session
  :config
  (setq session-use-package t)
  (session-initialize)
  (add-to-list 'session-globals-exclude 'org-mark-ring)
  (add-to-list 'session-globals-exclude 'consult--buffer-history))

(use-package fill-column-indicator
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

;; Treemacs for each workspace
(use-package treemacs
  :config
  (treemacs-resize-icons 16)
  (treemacs-follow-mode)
  (treemacs-git-mode 'deferred)
  :defer t)

(use-package treemacs-all-the-icons
  :after (treemacs))

;; Treemacs integrations with Magit and Projectile
(use-package treemacs-magit
  :after (treemacs magit))
(use-package treemacs-projectile
  :after (treemacs projectile))

;; Buffer Move: I actually want to swap buffers around windows sometimes, so this is useful...
(use-package buffer-move
  :bind
  (([C-S-s-up]      . buf-move-up)
   ([C-S-s-down]    . buf-move-down)
   ([C-S-s-left]    . buf-move-left)
   ([C-S-s-right]   . buf-move-right)))

;; ace-jump
(use-package ace-jump-mode
  :bind
  ((:map jmi/my-jump-keys-map
         ("SPC" .   ace-jump-mode))))

(use-package ace-window
  :config
  (setq aw-background 't)

  :bind
  (("M-`"   .  ace-window)))

;; Ensure window config can be undone
(use-package winner
  :config
  (winner-mode)
  :ensure nil)

;; Breadcrumbs
(use-package breadcrumb
  :config
  (breadcrumb-mode)
  :demand t)


;;; navigation.init.el ends here
