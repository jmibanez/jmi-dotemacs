;;; encryption.init.el -- Config for encryption, etcetera
;;; Commentary:

;;; Code:

(use-package epg
  :ensure nil ;; System package

  :config
  (setq epg-pinentry-mode 'loopback))

(use-package epa-file
  :ensure nil ;; System package

  :custom
  (epg-gpg-program (concat jmi/homebrew-binary-path "gpg"))

  :config
  (epa-file-enable))

;;; encryption.init.el ends here
