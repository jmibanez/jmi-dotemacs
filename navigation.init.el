;;; navigation.init.el -- Navigation and movement in Emacs  -*- lexical-binding: t; -*-

;;; Commentary:
;;; This configures a bunch of stuff I use for getting around my Emacs
;;; instance, including windmove (using a custom set of bindings) and
;;; projectile.  Also included is a bare helm config here, which I'll
;;; populate later on as I get used to using helm

;;; Code:

;; display-buffer tweaks
(setopt switch-to-buffer-obey-display-actions t)
(setopt switch-to-buffer-in-dedicated-window 'pop)

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
  (setopt projectile-completion-system 'auto)

  :config
  (setopt projectile-indexing-method 'hybrid
          projectile-project-root-functions
          '(projectile-root-local
            projectile-root-bottom-up
            projectile-root-marked
            projectile-root-top-down
            projectile-root-top-down-recurring)

          projectile-project-search-path
          '(("~/projects/" . 2))

          projectile-globally-ignored-files
          '(".classpath"
            ".factorypath"
            ".python-version"
            ".project")

          projectile-globally-ignored-file-suffixes
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
  :defer t
  :config
  (savehist-mode))

;; Vertico
(use-package vertico
  :config
  (setopt vertico-count  25
          vertico-resize t)

  (vertico-mode)
  :demand t)

(use-package counsel
  :defer t)

(use-package consult
  :config
  (setopt consult-preview-key nil)

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
  :config
  ;; Define orderless style with initialism by default -- shamelessly
  ;; stolen from:
  ;; https://github.com/minad/consult/wiki#minads-orderless-configuration
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  (setopt completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion))
                                          (command (styles +orderless-with-initialism))
                                          (variable (styles +orderless-with-initialism))
                                          (symbol (styles +orderless-with-initialism)))
          orderless-component-separator #'orderless-escapable-split-on-space))

(use-package popup
  :config
  (setopt popup-tip-max-width 100))

(use-package posframe)
(use-package vertico-posframe
  :config

  ;; Shim for latest vertico which renamed vertico--resize-window to vertico--resize
  (cl-defmethod vertico--resize
    (&context ((vertico-posframe-mode-workable-p) (eql t))))

  (vertico-posframe-mode)
  :after posframe)

(use-package framemove
  :ensure nil
  :defer t
  :config
  (setq framemove-hook-into-windmove t))

(use-package windmove
  :ensure nil
  :config
  (require 'framemove)

  ;; Window move via cmd-ctrl-(vim keys)
  ;; Changed to be consistent with how we've bound things in our iTerm2 config
  :bind (("C-s-h"        . windmove-left)
         ("C-s-l"        . windmove-right)
         ("C-s-k"        . windmove-up)
         ("C-s-j"        . windmove-down)))

;; Use Emacs session management
(use-package session
  :defer t
  :config
  (setopt session-use-package t)
  (session-initialize)
  (add-to-list 'session-globals-exclude 'org-mark-ring)
  (add-to-list 'session-globals-exclude 'consult--buffer-history))

;; Bind F7/S-F7 to bookmark
(use-package bookmark
  :defer t
  :ensure nil ;; System package
  :bind (("<f7>"         .  bookmark-jump)
         ("S-<f7>"       .  bookmark-set)))

;; Fullscreen
(use-package frame
  :defer t
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
  (setopt aw-background 't)

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
  (breadcrumb-mode))

;; Configure dired defaults
(use-package dired
  :ensure nil
  :defer t

  :config
  ;; Taken from https://emacs.dyerdwelling.family/emacs/20250513085926-emacs--instantly-open-dired-files-with-isearch-and-enter/
  (defun jmi/dired-isearch-dwim ()
    "In dired mode, enter directory or open file after isearch."
    (when (eq major-mode 'dired-mode)
      (let ((file (dired-get-file-for-visit)))
        (when file
          (dired-find-file)))))

  (advice-add 'isearch-exit :after
              #'jmi/dired-isearch-dwim)

  :hook ((dired-mode . dired-omit-mode)))

;;; navigation.init.el ends here
