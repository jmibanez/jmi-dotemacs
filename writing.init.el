;;; writing.init.el -- Stuff to help make it more pleasant to write prose

;;; Commentary:
;;; These are a bunch of stuff that I use to help make writing prose
;;; easier -- mostly my blog posts, occasionally a technical document
;;; or two.

;;; Code:

;; Switch to using enchant as our spell-checking backend (fallback to ispell)
(setq ispell-program-name
      (or (executable-find "enchant")
          (executable-find "ispell")
          "ispell"))

;; Use langtool for grammar checking; ensure languagetool exists in
;; system
(use-package langtool
  :config
  (setq langtool-bin
        (or (executable-find "languagetool")
            "languagetool"))

  :ensure-system-package languagetool)



;; AucTeX - I have some LaTeX files (for instance, my CV and resume),
;; so it makes sense to have this installed
(use-package tex-site
  :ensure auctex)

;; git-auto-commit-mode -- for automatically distributing my drafts on my blog to Git
(use-package git-auto-commit-mode)


;; PlantUML
(use-package plantuml-mode)

(use-package writegood-mode)

(use-package olivetti)

;; Should these be here? Maybe move to separate init file?
(use-package markdown-mode
  :config
  (defun jmi/setup-markdown-writing-settings ()
    (interactive)
    (set-fill-column 120)
    (visual-line-mode)
    (olivetti-mode)
    (writegood-mode)
    (whitespace-mode 0))

  :hook ((markdown-mode   . jmi/setup-markdown-writing-settings))

  :after (writegood-mode olivetti))


;;; writing.init.el ends here
