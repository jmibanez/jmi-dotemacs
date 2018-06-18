;;; whitespace.init.el -- Whitespace handling
;;; Commentary:

;;; Code:
(use-package whitespace
  :init
  (setq whitespace-style
        '(face
          lines-tail
          trailing
          empty))
  :config
  (global-whitespace-mode))

;; Ensure tabs are expanded, not inserted
(setq-default indent-tabs-mode nil)

;;; whitespace.init.el ends here
