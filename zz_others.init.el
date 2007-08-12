;; For pymacs-- am using it for developing Ozone

(setq jmibanez-pymacs-dir (expand-file-name "~/pymacs/"))

(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path jmibanez-pymacs-dir))

(defun jmlisp-maybe-pymacs-reload ()
  (let ((pymacsdir jmibanez-pymacs-dir))
        (when (and (string-equal (file-name-directory buffer-file-name)
                                 pymacsdir)
                   (string-match "\\.py\\'" buffer-file-name))
          (pymacs-load (substring buffer-file-name 0 -3)))))

(add-hook 'after-save-hook 'jmlisp-maybe-pymacs-reload)


;; Other init
(require 'ljupdate)
(require 'git)
(require 'vc-git)
(add-to-list 'vc-handled-backends 'git)

;; Color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-charcoal-black-ext)


;;(require 'planner)
;;(require 'planner-notes-index)
;;(require 'planner-rss)
;;(require 'planner-calendar)
;;(require 'planner-diary)
;;(require 'planner-cyclic)

;;(global-font-lock-mode)
;; (auto-load-library psvn)
;; (auto-load-library vc-svn)

;; Annoyance: Fix to 'Buffer %s not set up for parsing' stuff
;; (setq global-senator-minor-mode t)

;; Load longlines
;;(require 'longlines)

;; Extended page handling
(require 'page-ext)
(put 'narrow-to-page 'disabled nil)

;; Turn on iswitchb
(iswitchb-mode 1)

;; (mouse-wheel-mode)
;; (setq pop-up-windows nil)

;; F10 pipe the message as spam
;; (global-set-key [(f10)]
;;   '(lambda () (interactive) (shell-command "sa-learn --spam --single ")))
;; 
;; ;; F9 pipe the message as ham
;; (global-set-key [(f9)]
;;   '(lambda () (interactive) (shell-command "sa-learn --ham --single ")))

;;(require 'java-mode-indent-annotations)
;;(add-hook 'jde-mode-hook #'java-mode-indent-annotations-setup)


(setq g-user-email "jmibanez@gmail.com")

(require 'flymake)
(add-hook 'find-file-hooks 'flymake-find-file-hook)
(require 'jde-eclipse-compiler-server)
;; (jde-flymake-jikes-app-name)

(setq midnight-mode t)


