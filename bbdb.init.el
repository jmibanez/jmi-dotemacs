;; BBDB Config

(require 'bbdb-autoloads)
(require 'bbdb)

(bbdb-initialize 'gnus 'message)

;;If you don't live in Northern America, you should disable the 
;;syntax check for telephone numbers by saying
(setq bbdb-north-american-phone-numbers-p nil)

;;Tell bbdb about your email address:
(setq bbdb-user-mail-names
      (regexp-opt '("jmibanez@gmail.com"
                    "jm@orangeandbronze.com")))
;;cycling while completing email addresses
(setq bbdb-complete-name-allow-cycling t)
;;No popup-buffers
(setq bbdb-use-pop-up nil)

;; Autonotes
(setq bbdb-auto-notes-alist
      '(("To"
         ("plug" . "plug")
         ("pinoyjug" . "pinoyjug")
         ("linux" . "linux")
         ("xorg" . "xorg")
         ("git" . "git")
         ("emacs" . "emacs")
         ("dashboard-hackers" . "beagle/dashboard")
         ("jmibanez" . "personal mail"))
        ("From"
         ("orangeandbronze" . "Orange & Bronze Software Labs Ltd."))
        ("Organization" (".*" company 0 nil))))

(setq bbdb-auto-notes-ignore '(("Organization" . "^Gatewayed from\\\\|^Source only")
                               ("Group" . "^nnrss")))
(setq bbdb-auto-notes-ignore-all nil)
(setq bbdb-default-area-code 632)
(setq bbdb-default-country "Philippines")
(setq bbdb-ignore-some-messages-alist
      '(("From" . "hotmail")
        ("From" . "yahoo.com")
        ("From" . "adler.orangeandbronze.com")
        ("From" . "facebook.com")
        ("From" . "facebookmail.com")
        ("From" . "noreply")
        ("From" . "do-not-reply@onbsl.grouphub.com")
        ("From" . "trac@orangeandbronze.com")
        ("From" . "svn@orangeandbronze.com")
        ("From" . "projects@orangeandbronze.com")
        ("From" . "sysads@orangeandbronze.com")
        ("Message-Id" . ".nnrss>$")))
(setq bbdb-notice-hook '(bbdb-auto-notes-hook))
(setq bbdb/mail-auto-create-p (quote bbdb-ignore-some-messages-hook))
(setq bbdb/news-auto-create-p (quote bbdb-ignore-some-messages-hook))

(setq bbdb-offer-save 'savenoprompt)

