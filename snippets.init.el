;;; snippits.init.el -- YASnippet configuration

;;; Commentary:
;;; Separate YASnippet into a separate config file since I'll be
;;; using it for writing templates as well

;;; Code:

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        `(,(concat jmi/my-emacs-init-path "snippets"))))

(use-package consult-yasnippet
  :defer t
  :after yasnippet)

(use-package yasnippet-snippets
  :config
  (yas-global-mode)
  (yasnippet-snippets-initialize))

;;; snippets.init.el ends here
