;;; misc.init.el -- Unsorted, miscellaneous config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; This is a catch-all file for miscellaneous bits of config

;;; Code:

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Make kill ring larger
(setopt kill-ring-max 1000)

;; Sentences end with a single space.
(setopt sentence-end-double-space nil)

(use-package page-ext
  :defer t
  :init
  (put 'narrow-to-page 'disabled nil)
  :ensure nil)

;; At midnight, cleanup
(use-package midnight
  :config
  (midnight-mode)
  :ensure nil)

;; symon
(use-package symon
  :defer t)

;; Nyan cat! (Included just for fun)
(use-package nyan-mode
  :defer t)

;; Unfill paragraph

;;; From EmacsWiki via Stefan Monnier <foo at acm.org>. It is the
;;; opposite of fill-paragraph
(defun jmi/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; wttrin
(use-package wttrin
  :config
  (setopt wttrin-default-locations
          '("Sydney, AU" "Manila, PH" "New York, NY" "San Francisco, CA" "London, UK")
          wttrin-font-name "Berkeley Mono")

  ;; (setq jmi/default-wttrin-location "Sydney, AU")

  :bind
  ((:map jmi/my-jump-keys-map
         ("w"    .   wttrin))))

;; Run unison-daemon for all profiles in background
(use-package unison-daemon
  :defer t
  :vc (:url "https://github.com/jmibanez/unison-daemon-el"
       :branch "main"
       :rev :newest))

;; Automatically update packages
(use-package auto-package-update
  :config
  ;; Hide results
  (setopt auto-package-update-hide-results 't)
  ;; ... at startup
  (auto-package-update-maybe)
  ;; ... specifically check at noon
  (auto-package-update-at-time "12:00"))

(use-package emacs
  :ensure nil
  :config
  (defun jmi/ask-user-about-lock-dwim-with-hostname (orig-ask-user-fn file other-user)
    (let ((current-hostname-no-suffix (car (split-string (system-name) "\\.")))
          (other-user-hostname-no-suffix
           (car (split-string (cadr (split-string other-user
                                                  "@"))
                              "\\.")))
          (current-user (user-login-name))
          (other-user-user (car (split-string other-user "@")))
          (current-session-pid (number-to-string (emacs-pid)))
          (other-user-pid (last (split-string (car (split-string other-user ":"))
                                              "\\."))))
      (or (and (string-equal current-hostname-no-suffix other-user-hostname-no-suffix)
               (string-equal current-user other-user-user)
               (string-equal current-session-pid other-user-pid))
          (funcall orig-ask-user-fn file other-user))))

  (advice-add 'ask-user-about-lock :around
              #'jmi/ask-user-about-lock-dwim-with-hostname)

  :if (eq system-type 'darwin))

;;; misc.init.el ends here
