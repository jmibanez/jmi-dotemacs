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

(use-package w3m
  :config
  (defun jmi/w3m-view-url-via-browse-url ()
    (interactive)
    (browse-url (w3m-anchor)))

  :bind
  ((:map w3m-minor-mode-map
         ("u" . jmi/w3m-view-url-via-browse-url))))


(use-package bbdb)


(use-package gnus
  :init
  (add-to-list 'command-switch-alist
               '("gnus" . (lambda (&rest ignore)
                            ;; When given -gnus, start Gnus when Emacs starts
                            (add-hook 'emacs-startup-hook 'gnus t)
                            ;; Exit Emacs after Gnus quits
                            (add-hook 'gnus-after-exiting-gnus-hook
                                      'save-buffers-kill-emacs))))

  :config
  (setq gnus-parameters
        '(("INBOX" (nov-cache-size . 5000))
          ("INBOX\\.Notifications\\.*" (nov-cache-size . 7000))))

  (setq gnus-nov-is-evil t)

  ;; Primary: jmibanez.com
  (setq gnus-select-method
        '(nnimap "jmibanez"
                 (nnimap-address        "imap.gmail.com")
                 (nnimap-stream         ssl)
                 (nnimap-server-port    "imaps")))
  ;; Search via mairix
  ;; NB: We need to require nnmairix to load it and have its keybindings active
  (require 'nnmairix)
  (setq gnus-secondary-select-methods
        '((nnmaildir "mairix"
                     (directory     "~/.nnmairix"))
          (nnml      "archive"
                     (nnml-directory             "~/Mail.archive")
                     ;; Don't expire messages in the archive!
                     (nnml-inhibit-expiry        t))
          (nnimap "gmail"
                  (nnimap-address       "imap.gmail.com")
                  (nnimap-stream        ssl)
                  (nnimap-server-port   "imaps"))))


  (setq gnus-group-line-format "%M%S%p%5y:%B%(%G%)%O\n")
  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "Today, %H:%M")
          ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
          (604800 . "%a %H:%M")
          ((gnus-seconds-month) . "%a %d")
          ((gnus-seconds-year) . "%b %d")
          (t . "%b %d %Y")))

  (setq gnus-summary-line-format "%~(pad-right 20)&user-date; %U%R%z%(%[%4L: %-23,23f% ]%)%B %s\n")

  ;; Enable Gnus agent
  (setq gnus-agent t)

  ;; HTML renderer
  (setq mm-text-html-renderer 'w3m)
  ;; Make sure to open URLs externally
  (setq mm-url-use-external t)

  (setq mm-discouraged-alternatives
        '("text/html" "text/richtext" "text/enriched"))

  ;; Frame configuration/layout
  (setq gnus-use-full-window t)
  (gnus-add-configuration
   '(article
     (vertical 1.0
               (group 0.12)
               (summary 0.25 point)
               (article 1.0))))
  (gnus-add-configuration
   '(summary
     (vertical 1.0
               (group 0.12)
               (summary 1.0 point))))

  ;; Scoring
  ;; Adaptive scoring
  (setq gnus-use-adaptive-scoring '(word line))
  (setq gnus-default-adaptive-score-alist
        '((gnus-unread-mark)
          (gnus-ticked-mark (from 4))
          (gnus-dormant-mark (from 7))
          (gnus-del-mark (from -4) (subject -15))
          (gnus-read-mark (from 5) (subject 30))
          (gnus-expirable-mark (from -3) (subject -3))
          (gnus-killed-mark (from -4) (subject -20))
          (gnus-kill-file-mark)
          (gnus-ancient-mark (subject -10))
          (gnus-low-score-mark)
          (gnus-catchup-mark (from -1) (subject -10))))


  ;; Make sure to not score short (3 or fewer letter) words
  (setq gnus-adaptive-word-length-limit 4)

  ;; Don't include words in the group
  (setq gnus-adaptive-word-no-group-words t)

  ;; Threads and thread handling
  ;; Thread display chars
  (setq gnus-sum-thread-tree-root             "="
        gnus-sum-thread-tree-false-root       "< "
        gnus-sum-thread-tree-single-indent    " "
        gnus-sum-thread-tree-indent           "  "
        gnus-sum-thread-tree-leaf-with-other  "+-> "
        gnus-sum-thread-tree-single-leaf      "`-> "
        gnus-sum-thread-tree-vertical         "| ")

  ;; Sort threads by score before date (descending, recent first)
  (setq gnus-thread-sort-functions
        '(gnus-thread-sort-by-score
          (not gnus-thread-sort-by-date)
          gnus-thread-sort-by-total-score))

  ;; Fetch some older messages in the thread, for context
  (setq gnus-fetch-old-headers 'some)

  ;; Jump to first link in w3m-washed article
  (defun jmi/gnus-summary-forward-link (n)
    (interactive "p" gnus-summary-mode)
    (gnus-summary-select-article)
    (gnus-configure-windows 'article)
    (let ((win (or (gnus-get-buffer-window gnus-article-buffer t)
                   (error "No article window found"))))
      (select-window win)
      (select-frame-set-input-focus (window-frame win))
      (if (fboundp 'w3m-minor-mode)
          (w3m-next-anchor n)
        (forward-button n))))

  (defun jmi/gnus-summary-browse-link-forward (n)
    (interactive "p" gnus-summary-mode)
    (gnus-summary-select-article)
    (gnus-configure-windows 'article)
    (let ((win (or (gnus-get-buffer-window gnus-article-buffer t)
                   (error "No article window found"))))
      (select-window win)
      (select-frame-set-input-focus (window-frame win))
      (if (fboundp 'w3m-minor-mode)
          (progn
            (w3m-next-anchor n)
            (browse-url (w3m-anchor)))

        (browse-url (forward-button n)))))

  ;; Posting styles
  (setq gnus-posting-styles
        '((".*"
           (name            "JM Ibanez")
           (address         "jm@jmibanez.com")
           (signature-file  "~/.signature"))))

  :bind ((:map jmi/my-jump-keys-map
               ("m"       . gnus))
         (:map gnus-summary-mode-map
               ("TAB"     . jmi/gnus-summary-forward-link)
               ("C-<tab>" . jmi/gnus-summary-browse-link-forward))
         (:map gnus-article-mode-map
               ("TAB"     . jmi/gnus-summary-forward-link)))

  :ensure nil
  :after (jmi-keybindings))

(use-package gnus-notifications
  :config
  ;; Notifications config

  ;; First, we need to disable gravatar + google contacts, as that
  ;; leaks internal email addresses
  (setq gnus-notifications-use-google-contacts      nil
        gnus-notifications-use-gravatar             nil)

  ;; Replace gnus-notifications-notify with our own custom function,
  ;; which exec's terminal-notifier (FIXME: Make this macOS specific
  ;; only!)
  (defun jmi/gnus-notification-terminal-notify (from subject ignored-photo-file)
    "Override for gnus-notification-notify"
    (let ((washed-subject (format "Subject: %s" subject)))
      (call-process "/usr/local/bin/terminal-notifier" nil nil nil
                    "-sound"    "Blow"
                    "-title"    from
                    "-message"  washed-subject)))

  (advice-add 'gnus-notifications-notify
              :override #'jmi/gnus-notification-terminal-notify)

  :hook
  ((gnus-after-getting-new-news   .  gnus-notifications))

  :ensure nil
  :after gnus)

(use-package gnus-topic
  :hook
  ((gnus-group-mode               .  gnus-topic-mode))

  :ensure nil
  :after gnus)


;;; communication.init.el ends here
