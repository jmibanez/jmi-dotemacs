;;; communication.init.el -- Chat and comms

;;; Commentary:
;;; RSS feed readers, IRC, in-Emacs Twitter support, etcetera.

;;; Code:

(use-package elfeed
  :ensure t

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
          ("http://feedpress.me/sixcolors" blog apple)))


  :config
  (defun jmi/use-bigger-elfeed-font ()
    (set-face-attribute 'variable-pitch (selected-frame)
                        :font (font-spec :family "helvetica" :size 12)))

  :hook (elfeed-show-mode . jmi/use-bigger-elfeed-font)

  :bind (("<f8> n"  .  elfeed)))


(use-package circe
  :ensure t)

(use-package slack
  :ensure t

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

;;; communication.init.el ends here
