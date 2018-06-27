;;; communication.init.el -- Chat and comms

;;; Commentary:
;;; RSS feed readers, IRC, in-Emacs Twitter support, etcetera.

;;; Code:

(use-package elfeed
  :init
  (setq elfeed-feeds
        '(("https://blog.jmibanez.com/feed.xml" blog)
          ("https://xkcd.com/atom.xml" comics)
          ("http://leancrew.com/all-this/feed/" blog)
          ("https://daringfireball.net/feeds/main" blog apple)
          ("http://feeds.arstechnica.com/arstechnica/index" news)
          ("https://www.jwz.org/blog/feed/" blog)
          ("https://marco.org/rss" blog apple dev)
          ("https://programmingisterrible.com/rss" blog dev)
          ("http://prog21.dadgum.com/atom.xml" blog dev)
          ("https://donmelton.com/rss.xml" blog apple dev)
          ("http://james-iry.blogspot.com/feeds/posts/default" blog dev)
          ("https://fishbowl.pastiche.org/atom.xml" blog dev)
          ("http://feedpress.me/sixcolors" blog apple)
          ("http://nullprogram.com/feed/" blog dev emacs)
          ("http://hownow.brownpau.com/feed" blog)))

  (setq jmi/default-elfeed-search-filter "@2-weeks-ago +unread")

  :config
  (setq-default elfeed-search-filter jmi/default-elfeed-search-filter)
  (defun jmi/use-bigger-elfeed-font ()
    (set-face-attribute 'variable-pitch (selected-frame)
                        :font (font-spec :family "helvetica" :size 12)))

  (defun jmi/elfeed-jump-to-bookmark-entries ()
    (interactive)
    (elfeed-search-set-filter "+bookmark"))

  (defun jmi/elfeed-jump-to-latest-unread-entries ()
    (interactive)
    (elfeed-search-set-filter jmi/default-elfeed-search-filter))

  (defun jmi/elfeed-search-bookmark-all ()
    (interactive)
    (elfeed-search-tag-all 'bookmark))

  (defun jmi/elfeed-show-bookmark-entry ()
    (interactive)
    (elfeed-show-tag 'bookmark))

  :hook (elfeed-show-mode . jmi/use-bigger-elfeed-font)

  :bind ((:map jmi/my-jump-keys-map
               ("n"       .  elfeed))
         (:map elfeed-search-mode-map
               ("<f7>"    .  jmi/elfeed-jump-to-bookmark-entries)
               ("S-<f7>"  .  jmi/elfeed-search-bookmark-all)
               ("*"       .  jmi/elfeed-jump-to-latest-unread-entries))
         (:map elfeed-show-mode-map
               ("S-<f7>"  .  jmi/elfeed-show-bookmark-entry)))

  :after jmi-keybindings)


(use-package circe)

;; Use circe with Helm
(use-package helm-circe
  :after (helm circe))

(use-package slack
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)

  :config
  (defun jmi/get-slack-auth (slack-team)
    "Get relevant OAuth client-ID and client-secret from SLACK-TEAM."
    (let* ((slack-host (format "%s.slack.com" slack-team))
           (auth (car (auth-source-search :host slack-host
                                          :max 1)))
           (client-id-and-secret (s-split "\\^" (plist-get auth :user)))
           (token (funcall (plist-get auth :secret))))
      (cons client-id-and-secret token)))

  (defun jmi/define-and-register-team (slack-team)
    (let* ((slack-auth    (jmi/get-slack-auth slack-team))
           (client-id     (car (car slack-auth)))
           (client-secret (cadr (car slack-auth)))
           (token         (cdr slack-auth)))
      (slack-register-team :name           slack-team
                           :client-id      client-id
                           :client-secret  client-secret
                           :token          token)))

  (mapc 'jmi/define-and-register-team
        '("phackers"))

  :commands slack-start)


;; WanderLust for mail
(use-package wanderlust

  :init
  (setq elmo-imap4-default-server "imap.gmail.com"
        elmo-imap4-default-authenticate-type 'clear
        elmo-imap4-default-port '993
        elmo-imap4-default-stream-type 'ssl

        ;; For non ascii-characters in folder-names
        elmo-imap4-use-modified-utf7 t

        ;; For the love of everything good and just, please use
        ;; auth-source for passwords
        elmo-passwd-storage-type 'auth-source

        mime-view-text/html-previewer 'shr

        ;; SMTP
        wl-smtp-connection-type 'starttls
        wl-smtp-posting-port '587
        wl-smtp-authenticate-type "plain"
        wl-smtp-posting-server "smtp.gmail.com"
        wl-smtp-connection-type 'starttls
        wl-message-id-domain "smtp.gmail.com"

        wl-draft-reply-buffer-style 'full
        wl-summary-toggle-mime "mime"
        mime-edit-split-message nil

        wl-message-visible-field-list '("^From" "^To" "^Subject" "^Date" "^Cc")
        wl-message-ignored-field-list '("^")

        wl-summary-showto-folder-regexp ".*"
        wl-summary-from-function 'wl-summary-default-from

        wl-folder-check-async t

        ;; Change the default summary line format to be more readable
        wl-summary-default-number-column 4
        wl-summary-width 100
        wl-summary-line-format "%n %T%P %Y-%M-%D %h:%m (%W) | %t %[%20(%f%)%]%C %s"

        ;; Seriously, the default threshold is too low, considering
        ;; modern networks; up it to at least 0.75 MB / 768 KB
        elmo-message-fetch-threshold 786432
        wl-message-buffer-prefetch-threshold 786432

        ;; Multi-folder (to aggregate my inboxes): set max to
        ;; something "absurdly" large -- 300k messages
        elmo-multi-max-number 300000

        ;; Prefetch up to 5 messages
        wl-message-buffer-prefetch-depth 5

        ;; All system folders (draft, trash, spam, etc) are placed in the
        ;; [Gmail]-folder, except inbox. "%" means it's an IMAP-folder
        wl-default-folder "%INBOX"
        wl-draft-folder   "%[Gmail]/Drafts"
        wl-trash-folder   "%[Gmail]/Trash"
        ;; The below is not necessary when you send mail through Gmail's SMTP server,
        ;; see https://support.google.com/mail/answer/78892?hl=en&rd=1
        ;; wl-fcc            "%[Gmail]/Sent"

        ;; Mark sent messages as read (sent messages get sent back to you and
        ;; placed in the folder specified by wl-fcc)
        wl-fcc-force-as-read    t

        ;; For auto-completing foldernames
        wl-default-spec "%"

        ;; Notifications
        wl-biff-check-interval 60
)

  :config
  ;; Like with elfeed, boost the font-face on messages being
  ;; rendered via shr
  (defun jmi/use-bigger-wl-message-font ()
    (set-face-attribute 'variable-pitch (selected-frame)
                        :font (font-spec :family "helvetica" :size 12)))


  :bind ((:map jmi/my-jump-keys-map
               ("m"       . wl)))

  :after jmi-keybindings)

;;; communication.init.el ends here
