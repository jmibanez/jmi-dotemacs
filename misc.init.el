;;; misc.init.el -- Unsorted, miscellaneous config

;;; Commentary:

;;; This is a catch-all file for miscellaneous bits of config

;;; Code:

(use-package page-ext
  :init
  (put 'narrow-to-page 'disabled nil))

;; Pop-up windows when display-buffer
(setq pop-up-windows t)

;; At midnight, cleanup
(use-package midnight)

;; Indicate buffer boundaries
(setq-default indicate-buffer-boundaries '((top . left) (t . right)))

;;; misc.init.el ends here
