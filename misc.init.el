;;; misc.init.el -- Unsorted, miscellaneous config

;;; Commentary:

;;; This is a catch-all file for miscellaneous bits of config

;;; Code:

(use-package page-ext
  :init
  (put 'narrow-to-page 'disabled nil)
  :ensure nil)

;; At midnight, cleanup
(use-package midnight
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
        wttrin-default-accept-language
        '("Accept-Language" . "en-US,en;q=0.8")

        wttrin-font-name "Berkeley Mono"

        jmi/default-wttrin-location "Sydney, AU")

  (defun jmi/wttrin-fetch-raw-string (query)
    (let ((url-request-extra-headers '(("X-Emacs-Package" . "wttrin.el")))
          (url-user-agent "curl"))
      (add-to-list 'url-request-extra-headers wttrin-default-accept-language)
      (with-current-buffer
          (url-retrieve-synchronously
           (concat "http://wttr.in/" query)
           (lambda (status) (switch-to-buffer (current-buffer))))
        (decode-coding-string (buffer-string) 'utf-8))))


  (advice-add 'wttrin-fetch-raw-string
              :override #'jmi/wttrin-fetch-raw-string)

  :bind
  ((:map jmi/my-jump-keys-map
         ("w"    .   wttrin))))

;; Run unison-daemon for all profiles in background
(use-package unison-daemon
  :load-path "~/projects/personal/unison-daemon-el"

  :hook
  ((emacs-startup   . (lambda () (unison-daemon t)))))

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
