;;; development.init.el -- Various config for programming

;;; Commentary:
;;; This contains all of the config for various modes I use for
;;; writing software (including version control stuff); some of the
;;; Lisp modes stuff were shamelessly taken from Aaron Bedra's config
;;; (see http://aaronbedra.com/emacs.d/).

;;; Code:

;; Lisp Modes
(setq lisp-modes '(lisp-mode
                   emacs-lisp-mode
                   common-lisp-mode
                   scheme-mode
                   clojure-mode))


(use-package rainbow-delimiters)
(use-package paredit)

(defvar lisp-power-map (make-keymap))
(define-minor-mode lisp-power-mode "Fix keybindings; add power."
  :lighter " (power)"
  :keymap lisp-power-map
  (rainbow-delimiters-mode t)
  (paredit-mode t))
(define-key lisp-power-map [delete] 'paredit-forward-delete)
(define-key lisp-power-map [backspace] 'paredit-backward-delete)

(defun abedra/engage-lisp-power ()
  (lisp-power-mode t))

(dolist (mode lisp-modes)
  (add-hook (intern (format "%s-hook" mode))
            #'abedra/engage-lisp-power))

(setq inferior-lisp-program "clisp")
(setq scheme-program-name "mzscheme")

(use-package clojure-mode
  :config
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))

(defun jmi/cons-src-path-for-jvm-home (pair)
  "Return path for src.zip for JVM Home alist entry in PAIR"

  (let ((jvm-version (car pair))
        (jvm-path (cdr pair)))
    (if (equal jvm-version "1.8")
        (concat jvm-path "/src.zip")
      (concat jvm-path "/lib/src.zip"))))

(defun jmi/cons-jdk-src-paths (jvm-alist)
  "Construct JDK source paths (src.zip) in JVM-ALIST"

  (mapcar 'jmi/cons-src-path-for-jvm-home jvm-alist))

(use-package cider
  :init
  ;; (add-hook 'cider-mode-hook
  ;;           #'cider-turn-on-eldoc-mode)
  (setq nrepl-hide-special-buffers t)
  (setq cider-popup-stacktraces nil)
  (setq cider-repl-popup-stacktraces t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-jdk-src-paths (jmi/cons-jdk-src-paths jmi/jvm-homes-alist))
  (setq cider-jack-in-lein-plugins
        '(("refactor-nrepl" "2.4.0-SNAPSHOT" :predicate cljr--inject-middleware-p)
          ("cider/cider-nrepl" "0.18.0-SNAPSHOT")))
  ;; Disable refactor-nrepl and clj-refactor for now
  (setq cljr-inject-dependencies-at-jack-in t)

  :config
  ;; Shim to get slamhound working with latest CIDER -- alias
  ;; nrepl-send-string-sync to nrepl-sync-request:eval

  (defun nrepl-send-string-sync (s)
    (nrepl-sync-request:eval s
                             (cider-current-connection)
                             (cider-current-session)
                             (cider-current-ns)))

  :after
  jmi-init-platform-paths)

(use-package clj-refactor
  :after cider)

(use-package slamhound
  :after cider)

;; JS2 mode
(use-package js2-mode
  :mode "\\.js$")

;; Java dev/Eclim
(use-package eclim
  :init
  (setq eclimd-executable (concat jmi/eclipse-dir "eclimd"))
  (setq eclim-executable (concat jmi/eclipse-dir "eclim"))
  (setq eclim-eclipse-dirs jmi/eclipse-dir)

  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 1)

  (setq eclim-auto-save t)
  ;;  (add-to-list 'eclim--file-coding-system-mapping '("iso-latin-1-dos" . "ISO-8859-1"))

  :config
  (help-at-pt-set-timer)
  (global-eclim-mode)

  :after
  jmi-init-platform-paths)


;; Autocompletion helpers
(use-package auto-complete-config
  :ensure nil ;; System package

  :config
  (ac-config-default))

(use-package ac-emacs-eclim
  :config
  (ac-emacs-eclim-config)
  :after (auto-complete eclim))

(use-package ac-cider
  :config
  (ac-cider-setup)
  :after (auto-complete cider))

(use-package ac-html-bootstrap
  :after auto-complete)

(use-package ac-python
  :after auto-complete)

(use-package ac-js2
  :after js2-mode)

;; Go
(use-package go-mode)

(use-package go-autocomplete
  :after go-mode)

;; Python
(use-package python-django)
(use-package pyvenv)

;; Other languages/modes
(use-package groovy-mode
  :mode "\\.groovy$")

(use-package lua-mode
  :mode "\\.lua$")

(use-package coffee-mode
  :mode "\\.coffee$")

(use-package swift-mode
  :mode "\\.swift$")

(use-package thrift
  :mode "\\.thrift$")


;; Typescript IDE
(use-package tide)


;; Docker
(use-package dockerfile-mode
  :mode "Dockerfile")

;; Other text/config file modes
;; Should these be here? Maybe move to separate init file?
(use-package markdown-mode)

(use-package yaml-mode
  :mode "\\.yaml$")

;; Also include yasnippet
(use-package yasnippet)


;; Version control packages

;; Magit - Emacs interface to Git
(use-package magit
  :init
  ;; Point Magit to locally installed git (not system)
  (setq magit-git-executable jmi/git)

  ;; Set default magit dirs
  (setq magit-repo-dirs
        '("~/projects/personal"
          "~/projects/skunk"
          "~/projects/freelance"
          "~/projects/codeflux"))
  (setq magit-use-overlays nil)

  :after
  jmi-init-platform-paths)

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t))

(use-package magit-gitflow
  :after magit)

(use-package git-timemachine)

(use-package github-pullrequest)

(use-package github-notifier
  :config
  ;; github-notifier
  (setq github-notifier-token
        (funcall
         (plist-get (car (auth-source-search :host "api.github.com"
                                             :user "jmibanez^ghnotifier"
                                             :max 1
                                             :require '(:user :secret)
                                             :create t))
                    :secret)))
  :commands github-notifier)


(use-package magit-gh-pulls
  :hook (magit-mode . turn-on-magit-gh-pulls))

(use-package fullframe
  :config
  (fullframe magit-status magit-mode-quit-window nil))


;; Flycheck config
(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (declare-function python-shell-calculate-exec-path "python")

  (defun flycheck-virtualenv-set-python-executables ()
    "Set Python executables for the current buffer."
    (let ((exec-path (python-shell-calculate-exec-path)))
      (setq-local flycheck-python-pylint-executable
                  (executable-find "pylint"))
      (setq-local flycheck-python-flake8-executable
                  (executable-find "flake8"))))

  (defun flycheck-virtualenv-setup ()
    "Setup Flycheck for the current virtualenv."
    (when (derived-mode-p 'python-mode)
      (add-hook 'hack-local-variables-hook
                #'flycheck-virtualenv-set-python-executables 'local)))

  (provide 'flycheck-virtualenv)

  :after (pyvenv python-django))

;; Use Helm to browse Flycheck errors
(use-package helm-flycheck
  :after (helm flycheck))


;; SQL interaction stuff

;; Set up things so that doing M-x sql-postgres works the way I like
(setq sql-postgres-login-params
      '((user :default "jmibanez")
        (database :default "jmibanez")
        server
        (port :default 5432)))

;; Handle psql prompts where DB name has an underscore
;; \\(^\\w*=[#>] \\|^\\w*[-(][#>] \\)
(sql-set-product-feature
 'postgres :prompt-regexp "^\\(\\w\\|_\\)*=[#>]")
(sql-set-product-feature
 'postgres :prompt-cont-regexp "^\\(\\w\\|_\\)*[-(][#>]")

;;; allow-line-as-region-for-function adds an "-or-line" version of
;;; the given comment function which (un)comments the current line is
;;; the mark is not active.  This code comes from Aquamac's osxkeys.el
;;; and is licensed under the GPL

;; (taken from textmate.el -- jmibanez)

(defmacro allow-line-as-region-for-function (orig-function)
`(defun ,(intern (concat (symbol-name orig-function) "-or-line"))
   ()
   ,(format "Like `%s', but acts on the current line if mark is not active."
            orig-function)
   (interactive)
   (if mark-active
       (call-interactively (function ,orig-function))
     (save-excursion
       ;; define a region (temporarily) -- so any C-u prefixes etc. are preserved.
       (beginning-of-line)
       (set-mark (point))
       (end-of-line)
       (call-interactively (function ,orig-function))))))

(unless (fboundp 'comment-or-uncomment-region-or-line)
  (allow-line-as-region-for-function comment-or-uncomment-region))

(global-set-key [(super /)] #'comment-or-uncomment-region-or-line)

;;; development.init.el ends here
