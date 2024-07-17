;;; encryption.init.el -- Config for encryption, etcetera
;;; Commentary:

;;; Code:

(use-package epa-file
  :ensure nil ;; System package

  :custom
  (epg-gpg-program "/usr/local/bin/gpg")

  :config
  (epa-file-enable))

;;; encryption.init.el ends here
