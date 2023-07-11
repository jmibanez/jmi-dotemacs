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

;;; misc.init.el ends here
