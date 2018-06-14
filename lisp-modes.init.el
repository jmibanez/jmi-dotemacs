;;; lisp-modes.init.el -- Config for various Lisp dialects/languages

;;; Commentary:
;;; A bunch of things here were shamelessly taken from Aaron Bedra's config
;;; http://aaronbedra.com/emacs.d/

;;; Code:
(setq lisp-modes '(lisp-mode
                   emacs-lisp-mode
                   common-lisp-mode
                   scheme-mode
                   clojure-mode))

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

(use-package cider
  :init
  ;; (add-hook 'cider-mode-hook
  ;;           #'cider-turn-on-eldoc-mode)
  (setq nrepl-hide-special-buffers t)
  (setq cider-popup-stacktraces nil)
  (setq cider-repl-popup-stacktraces t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-jdk-src-paths
        '("/Library/Java/JavaVirtualMachines/jdk1.8.0_05.jdk/Contents/Home/src.zip"))
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
                             (cider-current-ns))))


;;; lisp-modes.init.el ends here
