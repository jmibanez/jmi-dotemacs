;;; tls.init.el -- enable SNI on tls-program

;;; Commentary:

;;; Code:

;; We assume Homebrew exists on this Mac, and that openssl is not
;; linked. Use all the versions in reverse order (try latest first)
(setq tls-program
      (mapcar (lambda (openssl-base)
                (concat openssl-base
                        "/bin/openssl"
                        " s_client -connect %h:%p -no_ssl3 -ign_eof"))
              (reverse
               (mapcan (lambda (brew-pkg)
                         (directory-files brew-pkg t
                                          "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)" nil))

                       (directory-files "/usr/local/Cellar/" t
                                        "openssl.*" nil)))))

;;; tls.init.el ends here
