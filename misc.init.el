;;; misc.init.el -- Unsorted, miscellaneous config

;;; Commentary:

;;; This is a catch-all file for miscellaneous bits of config

;;; Code:

(use-package page-ext
  :init
  (put 'narrow-to-page 'disabled nil))

;; At midnight, cleanup
(use-package midnight)

;; symon
(use-package symon)

;; Nyan cat! (Included just for fun)
(use-package nyan-mode)

;;; misc.init.el ends here
