;;; sql-postgres.init.el -- Config for sql-postgres

;;; Commentary:
;;; This just sets up things so that doing M-x sql-postgres works the
;;; way I like

;;; Code:

(setq sql-postgres-login-params
      '((user :default "jmibanez")
        (database :default "jmibanez")
        server
        (port :default 5432)))

;; \\(^\\w*=[#>] \\|^\\w*[-(][#>] \\)
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (setq sql-prompt-regexp
                  "^\\(\\w\\|_\\)*=[#>]")
            (setq sql-prompt-cont-regexp
                  "^\\(\\w\\|_\\)*[-(][#>]")))

;;; sql-postgres.init.el ends here
