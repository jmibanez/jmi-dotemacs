;;; tls.init.el -- enable SNI on tls-program

;;; Commentary:

;;; Code:

;; We assume Homebrew exists on this Mac, and that openssl is not
;; linked. Use all the versions in reverse order (try latest first)
(setq tls-program
      (mapcar (lambda (openssl-base)
                (concat openssl-base
                        "/bin/openssl"
                        " s_client -connect %h:%p -no_ssl2 -ign_eof"))
              (reverse
               (directory-files "/usr/local/Cellar/openssl/" t
                                "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)" nil))))

;;; tls.init.el ends here
