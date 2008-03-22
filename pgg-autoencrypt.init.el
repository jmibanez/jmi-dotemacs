(defvar pgg-gpg-user-id "JM Ibanez")
(autoload 'pgg-make-temp-file "pgg" "PGG")
(autoload 'pgg-gpg-decrypt-region "pgg-gpg" "PGG GnuPG")
(define-generic-mode 'gpg-file-mode
  (list ?#) 
  nil nil
  '(".gpg\\'" ".gpg-encrypted\\'")
  (list (lambda ()
          (add-hook 'before-save-hook
                    (lambda ()
                      (let ((pgg-output-buffer (current-buffer)))
                        (pgg-gpg-encrypt-region (point-min) (point-max)
                                                (list pgg-gpg-user-id))))
                    nil t)
          (add-hook 'after-save-hook
                    (lambda ()
                      (let ((pgg-output-buffer (current-buffer)))
                        (pgg-gpg-decrypt-region (point-min) (point-max)))
                      (set-buffer-modified-p nil)
                      (auto-save-mode nil))
                    nil t)
          (let ((pgg-output-buffer (current-buffer)))
            (pgg-gpg-decrypt-region (point-min) (point-max)))
          (auto-save-mode nil)
          (set-buffer-modified-p nil)))
  "Mode for gpg encrypted files")