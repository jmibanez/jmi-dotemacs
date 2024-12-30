;;; misc.init.el -- Unsorted, miscellaneous config

;;; Commentary:

;;; This is a catch-all file for miscellaneous bits of config

;;; Code:

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Sentences end with a single space.
(setq sentence-end-double-space nil)

(use-package page-ext
  :init
  (put 'narrow-to-page 'disabled nil)
  :ensure nil)

;; At midnight, cleanup
(use-package midnight
  :config
  (midnight-mode)
  :ensure nil)

;; symon
(use-package symon)

;; Nyan cat! (Included just for fun)
(use-package nyan-mode)

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
  (setq wttrin-default-locations
        '("Sydney, AU" "Manila, PH" "New York, NY" "San Francisco, CA" "London, UK")
        wttrin-font-name "Berkeley Mono"

        jmi/default-wttrin-location "Sydney, AU")

  :bind
  ((:map jmi/my-jump-keys-map
         ("w"    .   wttrin))))

;; Run unison-daemon for all profiles in background
(use-package unison-daemon
  :vc (:url "https://github.com/jmibanez/unison-daemon-el"
       :branch "main"
       :rev :newest))

;; Automatically update packages
(use-package auto-package-update
  :config
  ;; Hide results
  (setq auto-package-update-hide-results 't)
  ;; ... at startup
  (auto-package-update-maybe)
  ;; ... specifically check at noon
  (auto-package-update-at-time "12:00"))

;;; misc.init.el ends here
