;;; whitespace.init.el -- Whitespace handling  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(use-package whitespace
  :init
  (setopt whitespace-style
          '(face
            lines-tail
            trailing))
  :config
  (setopt whitespace-line-column nil

          ;; Don't enable in certain places, including Gnus etc.
          whitespace-global-modes
          '(not org-issues-mode
                org-issues-issue-mode
                org-issues-ticket-mode
                org-issues-sprint-mode
                org-issues-search-mode
                org-mode
                gnus-mode))

  (global-whitespace-mode)

  :ensure nil)

;; Ensure tabs are expanded, not inserted
(setq-default indent-tabs-mode nil)

;;; whitespace.init.el ends here
