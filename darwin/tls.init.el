;;; tls.init.el -- enable SNI on tls-program

;;; Commentary:

;;; Code:
(setq tls-program '("/usr/local/Cellar/openssl/1.0.2o_1/bin/openssl s_client -connect %h:%p -no_ssl2 -ign_eof"))

;;; tls.init.el ends here
