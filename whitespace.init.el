;;; whitespace.init.el -- Whitespace handling
;;; Commentary:

;;; Code:
(use-package whitespace
  :init
  (setq whitespace-style
        '(face
          lines-tail
          trailing))
  :config
  ;; Don't enable in certain places, including Gnus etc.
  (setq whitespace-global-modes
        '(not org-issues-mode
              org-issues-issue-mode
              org-issues-ticket-mode
              org-issues-sprint-mode
              org-issues-search-mode
              gnus-mode))

  (global-whitespace-mode)

  :ensure nil)

;; Ensure tabs are expanded, not inserted
(setq-default indent-tabs-mode nil)

;;; whitespace.init.el ends here
