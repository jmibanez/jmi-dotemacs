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


;;; writing.init.el ends here
