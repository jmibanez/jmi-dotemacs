;;; development.init.el -- Various config for programming

;;; Commentary:
;;; This contains all of the config for various modes I use for
;;; writing software (including version control stuff); some of the
;;; Lisp modes stuff were shamelessly taken from Aaron Bedra's config
;;; (see http://aaronbedra.com/emacs.d/).

;;; Code:

;; Tree Sitter

(use-package treesit
  :config
  (setq treesit-language-source-alist
        '((bash          "https://github.com/tree-sitter/tree-sitter-bash")
          (java          "https://github.com/tree-sitter/tree-sitter-java")
          (json          "https://github.com/tree-sitter/tree-sitter-json")
          (python        "https://github.com/tree-sitter/tree-sitter-python")
          (html          "https://github.com/tree-sitter/tree-sitter-html")
          (csharp        "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (go            "https://github.com/tree-sitter/tree-sitter-go")
          (go-mod        "https://github.com/camdencheek/tree-sitter-go-mod")
          (rust          "https://github.com/tree-sitter/tree-sitter-rust")
          (toml          "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx           "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript    "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))

  :ensure nil
  :defer t)

(use-package treesit-auto
  :config
  (setopt treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


;; Lisp Modes
(setq lisp-modes '(lisp-mode
                   emacs-lisp-mode
                   common-lisp-mode
                   scheme-mode
                   clojure-mode))

(use-package rainbow-delimiters)
(use-package paredit
  :config

  (defun jmi/paredit-C-j-if-not-lisp-interaction (&optional eval-last-sexp-arg-internal)
    "Run paredit-C-j unless we're in Lisp Interaction mode"
    (interactive "P")
    (if (eq 'lisp-interaction-mode
            major-mode)
        (eval-print-last-sexp eval-last-sexp-arg-internal)
      (paredit-C-j)))

  :bind
  (:map paredit-mode-map
        ("C-j" .  jmi/paredit-C-j-if-not-lisp-interaction)))

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

(setopt scheme-program-name "mzscheme")

;; SLIME config: SBCL + QuickLisp
(use-package slime
  :config
  (if (file-exists-p "~/.quicklisp/slime-helper.el")
      (load-file "~/.quicklisp/slime-helper.el"))

  (setopt inferior-lisp-program "sbcl")

  :bind (:map slime-mode-map
              ("M-TAB"  )))

(use-package slime-company
  :defer t)


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
    (context 2))
  :mode "\\.clj$")

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
  (setopt nrepl-hide-special-buffers      t
          cider-popup-stacktraces         nil
          cider-repl-popup-stacktraces    t
          cider-auto-select-error-buffer  t
          cider-jdk-src-paths             (jmi/cons-jdk-src-paths jmi/jvm-homes-alist)
          cider-jack-in-cljs-dependencies '(("cider/piggieback" "0.5.3")))

  :defer t
  :after
  (jmi-init-platform-paths clojure-mode))

(use-package clj-refactor
  :defer t
  :after cider)

;; JS2 mode
(use-package js2-mode
  :mode "\\.js$")

(use-package flymake
  :bind
  (("C-s-n"     . flymake-goto-next-error)
   ("C-s-p"     . flymake-goto-prev-error))

  :defer t
  :ensure nil)

(use-package flymake-diagnostic-at-point
  :config
  (defface :jmi-diagnostic-popup
    '((((type graphic))
       :background "#1640b0"
       :foreground "white"
       :slant italic
       :family "Berkeley Mono")
      (t
       :background bg-blue-intense
       :foreground white))
    "Customized face for Flymake diagnostic message popup")

  (defun jmi/flymake-diagnostic-at-point-via-popup (s)
    (popup-tip s
               :margin-left 1
               :margin-right 1
               :face :jmi-diagnostic-popup
               :width 80
               :truncate t))

  (setq flymake-diagnostic-at-point-display-diagnostic-function
        #'jmi/flymake-diagnostic-at-point-via-popup)

  :hook
  ((flymake-mode . flymake-diagnostic-at-point-mode))
  :after flymake)


(use-package rust-mode
  :defer t
  :config
  (setq rust-format-on-save t))

(use-package eglot
  :defer t

  :config
  ;; -- Some perf tweaks to make Eglot work "better"
  ;; Increase read-process size
  (setopt read-process-output-max (* 1024 1024))
  (setopt eglot-connect-timeout nil)

  (setq jmi/java-agent-lombok-arg (concat "-javaagent:" jmi/lombok-jar))

  ;; For downloading Lombok, url-copy-file in url
  (require 'url)
  (defun jmi/ensure-lombok-jar-exists ()
    (if (not (file-exists-p jmi/lombok-jar))
        (progn
          (mkdir (file-name-directory jmi/lombok-jar) t)
          (url-copy-file "https://projectlombok.org/downloads/lombok.jar" jmi/lombok-jar t))))

  (defun jmi/jdk-version-to-jdk-name (jdk-version-string)
    (let ((jdk-version-number (string-to-number jdk-version-string)))
      (cond ((>= jdk-version-number 9)
             (format "JavaSE-%s" (truncate jdk-version-number)))
            ((>= jdk-version-number 6)
             (format "JavaSE-1.%s" (truncate jdk-version-number)))
            (t (format "J2SE-%s" jdk-version-string)))))

  (defvar jmi/java-langserv-heap-min "100m"
    "Minimum heap size for Java language server instance")
  (defvar jmi/java-langserv-heap-max "8G"
    "Maximum heap size for Java language server instance")

  (defvar jmi/java-langserv-jvm-args
    `("--jvm-arg=-XX:+UseParallelGC"
      "--jvm-arg=-XX:GCTimeRatio=4"
      "--jvm-arg=-XX:AdaptiveSizePolicyWeight=90"
      "--jvm-arg=-Dsun.zip.disableMemoryMapping=true"
      ,(concat "--jvm-arg=-Xms" jmi/java-langserv-heap-min)
      ,(concat "--jvm-arg=-Xmx" jmi/java-langserv-heap-max))
    "JVM args for Java language server")

  (defvar jmi/java-langserv-init-options
    `(:settings
      (:java
       (:configuration
        (:runtimes ,(vconcat (mapcar (lambda (jvm-home-tuple)
                                       `(:name ,(jmi/jdk-version-to-jdk-name (car jvm-home-tuple))
                                               :path ,(cdr jvm-home-tuple)))
                                     jmi/jvm-homes-alist)))
        :updateBuildConfiguration t)
       :format (:enabled t :settings (:url ,(concat "file://" jmi/java-format-settings-file)
                                           :profile "NetDeps")))
      :extendedClientCapabilities (:classFileContentsSupport t)))

  (defun jmi/eglot-jdtls-args (interactive-p project)
    (append '("jdtls")
            jmi/java-langserv-jvm-args
            `(:initializationOptions ,jmi/java-langserv-init-options)))

  ;; Additional Eglot LSP config, specifically for -ts-mode variants
  (add-to-list 'eglot-server-programs
               `(java-ts-mode . ,#'jmi/eglot-jdtls-args))
  (add-to-list 'eglot-server-programs
               '(ruby-ts-mode "solargraph" "socket" "--port" :autoport))
  (add-to-list 'eglot-server-programs
               `(python-ts-mode
                 .  ,(eglot-alternatives
                     '("pylsp" "pyls" ("pyright-langserver" "--stdio") "jedi-language-server"))))

  (defun jmi/eglot-ensure-if-not-decompiled ()
    "Only ensure eglot is enabled if we're not opening a buffer in .eglot-java"
    (unless (string=
             ".eglot-java"
             (first (last (file-name-split
                           (file-name-directory default-directory)) 2)))
      (jmi/ensure-lombok-jar-exists)
      (eglot-ensure)))

  :ensure-system-package (jdtls pyright solargraph)

  :hook
  ((java-ts-mode     . jmi/eglot-ensure-if-not-decompiled)
   (python-ts-mode   . eglot-ensure)
   (rust-ts-mode     . eglot-ensure)
   (ruby-ts-mode     . eglot-ensure)))

(use-package eglot-java
  :config
  (setopt eglot-java-eglot-server-programs-manual-updates t)
  (eglot-java--jdthandler-patch-eglot)

  ;; Override eglot-java--find-server to just use eglot--current-server-or-lose
  (advice-add 'eglot-java--find-server :override
              #'eglot--current-server-or-lose)

  :after eglot)

(use-package kotlin-ts-mode
  :mode "\\.kts$")

(use-package java-ts-mode
  :config
  ;; Define custom java style
  (defconst jmi/hercules-java
    '((c-basic-offset . 4)     ; Guessed value
      (c-comment-only-line-offset . (0 . 0))
      (c-offsets-alist . ((inline-open . 0)
                          (topmost-intro-cont     . +)
                          (statement-block-intro  . +)
                          (knr-argdecl-intro      . 5)
                          (substatement-open      . +)
                          (substatement-label     . +)
                          (label                  . +)
                          (statement-case-open    . +)
                          (statement-cont         . +)
                          (arglist-intro          . ++)
                          (arglist-cont           . ++)
                          (arglist-close          . c-lineup-arglist)
                          (access-label           . 0)
                          (inher-cont             . c-lineup-java-inher)
                          (func-decl-cont         . c-lineup-java-throws)
                          (arglist-cont-nonempty  . ++))))
    "Eclipse Java Programming Style")
  (c-add-style "hercules-java" jmi/hercules-java)

  (defun jmi/java-mode-config ()
    ;; Set java-mode specific vars
    (setq-local fill-column 120)
    (setq-local whitespace-line-column 120)
    ;; Truncate lines, instead of wrapping
    (toggle-truncate-lines 1)
    (setq-local tab-width 4)
    (setq-local c-basic-offset 4))

  ;; Shadow java style
  (add-to-list 'c-default-style '(java-mode . "hercules-java"))

  (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))


  :hook
  (java-ts-mode . jmi/java-mode-config)

  :defer t
  :ensure nil) ;; built-in as of 29.1

;; Scala
(use-package scala-mode
  :defer t)

;; Autocompletion helpers
;; NB: Because we're switching to company-mode, we need to swap out some
;; stuff...

(use-package company
  :config
  (setopt company-frontends
          '(company-pseudo-tooltip-unless-just-one-frontend
            company-preview-frontend
            company-echo-metadata-frontend))

  :hook
  (after-init . global-company-mode))

(use-package dape
  :config
  (setopt dape-buffer-window-arrangement 'right)

  :after eglot)

;; Company backends
(use-package company-dict
  :defer t)
(use-package company-go
  :after go-mode)
(use-package company-shell
  :defer t)
(use-package company-sourcekit
  :defer t)
(use-package company-web
  :defer t)

(use-package company-quickhelp
  :defer t
  :config
  (company-quickhelp-mode))

;; Go
(use-package go-mode
  :defer t)

;; Python
(use-package python-django
  :defer t)
(use-package pyvenv
  :defer t)

(use-package python
  :defer t
  :ensure nil)

(use-package ruby-ts-mode
  :defer t
  :ensure nil) ;; built-in as of 29.1

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

(use-package protobuf-mode
  :mode "\\.proto")


;; Typescript IDE
(use-package tide
  :defer t)


;; Docker
(use-package dockerfile-mode
  :mode "Dockerfile")

;; Other text/config file modes

(use-package yaml-mode
  :mode "\\.yaml$")

(use-package perl-mode
  :defer t
  :ensure nil ;; System package
)

;; Version control packages

;; Magit - Emacs interface to Git
(use-package magit
  :init
  ;; Point Magit to locally installed git (not system)
  (setopt magit-git-executable jmi/git)

  :defer t
  :after
  'jmi-init-platform-paths)

(use-package git-link
  :bind
  ((:map jmi/my-jump-keys-map
         ("f l" .  git-link-dispatch))))

(use-package bug-reference
  :config

  :hook
  ((prog-mode   . bug-reference-mode))

  :ensure nil)

(use-package bug-reference-github
  :after (bug-reference))

(use-package git-gutter-fringe
  :config
  (setopt git-gutter-fr:side 'left-fringe)
  (global-git-gutter-mode t)

  :after
  magit)

(use-package magit-gitflow
  :after magit)

(use-package git-timemachine
  :defer t)

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


(use-package fullframe
  :config
  (fullframe magit-status magit-mode-quit-window nil))


;; SQL interaction stuff

(use-package sql
  :ensure nil
  :defer t

  :config
  ;; Set up things so that doing M-x sql-postgres works the way I like
  (setq sql-postgres-login-params
	'((user :default "jmibanez")
          (database :default "jmibanez")
          (port :default 5432)))

  ;; Handle psql prompts where DB name has an underscore
  ;; \\(^\\w*=[#>] \\|^\\w*[-(][#>] \\)
  (sql-set-product-feature
   'postgres :prompt-regexp "^\\(\\w\\|_\\)*=[#>]")
  (sql-set-product-feature
   'postgres :prompt-cont-regexp "^\\(\\w\\|_\\)*[-(][#>]"))

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
