;;; communication.init.el -- Chat and comms -*- lexical-binding: t; -*-

;;; Commentary:
;;; RSS feed readers, IRC, in-Emacs Twitter support, etcetera.

;;; Code:

(use-package elfeed
  :init
  (setopt elfeed-feeds
          `(("https://blog.jmibanez.com/feed.xml" blog)
            ("https://xkcd.com/atom.xml" comics)
            ("https://www.gpf-comics.com/rss/main_comic_rss.xml" comics)
            ("http://leancrew.com/all-this/feed/" blog)
            ("https://daringfireball.net/feeds/main" blog apple)
            (,(format "https://arstechnica.com/feed/?t=%s"
                      (auth-info-password (car (auth-source-search :host "arstechnica.com"
                                                                   :user "elfeed^jm@jmibanez.com"))))
             news)
            ("https://www.jwz.org/blog/feed/" blog)
            ("https://marco.org/rss" blog apple dev)
            ("https://mjtsai.com/blog/feed/" blog apple dev)
            ("https://programmingisterrible.com/rss" blog dev)
            ("http://prog21.dadgum.com/atom.xml" blog dev)
            ("https://donmelton.com/rss.xml" blog apple dev)
            ("https://irreal.org/blog/?feed=rss2" blog dev emacs)
            ("https://sachachua.com/blog/feed" blog dev emacs)
            ("http://james-iry.blogspot.com/feeds/posts/default" blog dev)
            ("https://fishbowl.pastiche.org/atom.xml" blog dev)
            ("http://feedpress.me/sixcolors" blog apple)
            ("http://nullprogram.com/feed/" blog dev emacs)
            ("http://hownow.brownpau.com/feed" blog)
            ("https://coredumped.dev/index.xml" blog dev emacs rust)))

  (setq jmi/default-elfeed-search-filter "@2-weeks-ago +unread")

  :config
  (setq-default elfeed-search-filter jmi/default-elfeed-search-filter)
  (defun jmi/use-bigger-elfeed-font ()
    (set-face-attribute 'variable-pitch (selected-frame)
                        :font (font-spec :family "helvetica" :size 14)))

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
               ("S-<f7>"  .  jmi/elfeed-show-bookmark-entry))))


(use-package circe
  :defer t)

(use-package w3m
  :config
  (defun jmi/w3m-view-url-via-browse-url ()
    (interactive)
    (browse-url (w3m-anchor)))

  :ensure-system-package w3m

  :bind
  ((:map w3m-minor-mode-map
         ("u" . jmi/w3m-view-url-via-browse-url))))


(use-package bbdb
  :ensure t
  :defer t)

(use-package xwwp
  :defer t
  :config
  (setopt xwwp-search-prefix "https://duckduckgo.com/?q=")

  :bind
  ((:map xwidget-webkit-mode-map
         ("l"   . xwwp-follow-link))))


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
  ;; Point to .gnus.el in this directory
  (setopt gnus-init-file (concat jmi/my-emacs-init-path ".gnus.el"))

  ;; Declaratively set up the nnmairix default search group and Search topic.
  ;; Mirrors what nnmairix-create-server-and-default-group does interactively,
  ;; but idempotently, so it is reproducible on a new machine.
  (defun jmi/gnus-setup-search-topic ()
    "Ensure the nnmairix default group and Search topic exist and are linked.
Idempotent: safe to run on every Gnus startup."
    (unless (gnus-group-entry "nnmairix+search:search")
      (nnmaildir-open-server "mairix")
      (gnus-group-make-group
       "search"
       (list 'nnmairix "search"
             (list 'nnmairix-backend        'nnmaildir)
             (list 'nnmairix-backend-server "mairix")
             (list 'nnmairix-mairix-command "mairix")
             (list 'nnmairix-hidden-folders nil)
             (list 'nnmairix-default-group  "search"))))
    (unless (gnus-topic-find-topology "Search")
      (gnus-topic-create-topic "Search" "Gnus" nil nil))
    (let ((group "nnmairix+search:search"))
      (when (and (gnus-group-entry group)
                 (not (equal (gnus-group-topic group) "Search")))
        (let ((old-entry (assoc (gnus-group-topic group) gnus-topic-alist)))
          (when old-entry
            (setcdr old-entry (delete group (cdr old-entry)))))
        (nconc (assoc "Search" gnus-topic-alist) (list group))
        (gnus-topic-enter-dribble))))

  (defun jmi/gnus-mail-search-in (query &rest groups-or-servers)
    "Search QUERY ephemerally across GROUPS-OR-SERVERS.
Each element is either a full Gnus group name containing \"+\"
\(e.g. \"nnmaildir+jmibanez.com:INBOX\") to restrict to that
group, or a server name in backend:name format without \"+\"
\(e.g. \"nnmaildir:jmibanez.com\") to search all groups on that
server."
    (let ((group-spec
           (mapcar (lambda (g)
                     (if (string-search "+" g)
                         (cons (gnus-method-to-server
                                (gnus-find-method-for-group g))
                               (list g))
                       (list g)))
                   groups-or-servers)))
      (gnus-group-read-ephemeral-group
       (concat "nnselect-" (message-unique-id))
       '(nnselect "nnselect")
       nil
       (cons (current-buffer) gnus-current-window-configuration)
       nil nil
       `((nnselect-specs
          (nnselect-function . gnus-search-run-query)
          (nnselect-args
           (search-query-spec (query . ,query))
           (search-group-spec . ,group-spec)))
         (nnselect-artlist)))))

  (defun jmi/gnus-mail-search-all-inboxes (query)
    "Search QUERY across all active inboxes (jmibanez.com + Gmail)."
    (interactive "sSearch (active inboxes): ")
    (jmi/gnus-mail-search-in query
                             "nnmaildir+jmibanez.com:INBOX"
                             "nnmaildir+gmail:INBOX"))

  (defun jmi/gnus-mail-search-jmibanez (query)
    "Search QUERY across all jmibanez.com groups."
    (interactive "sSearch (jmibanez.com): ")
    (jmi/gnus-mail-search-in query "nnmaildir:jmibanez.com"))

  (defun jmi/gnus-mail-search-gmail (query)
    "Search QUERY across all Gmail groups."
    (interactive "sSearch (Gmail): ")
    (jmi/gnus-mail-search-in query "nnmaildir:gmail"))

  (defun jmi/gnus-mail-search-archive-only (query)
    "Search QUERY in the nnml archive."
    (interactive "sSearch (archive only): ")
    (jmi/gnus-mail-search-in query "nnml:archive"))

  (defun jmi/gnus-mail-search-all (query)
    "Search QUERY across all mail, including archive."
    (interactive "sSearch (all): ")
    (jmi/gnus-mail-search-in query
                             "nnmaildir:jmibanez.com"
                             "nnmaildir:gmail"
                             "nnml:archive"))

  (defun jmi/get-notmuch-proc ()
    (let ((b (get-buffer "*notmuch new*")))
      (and (buffer-live-p b)
           (get-buffer-process b))))

  (defun jmi/callback-on-notmuch-update (proc change)
    ;; Rescan mail
    (gnus-group-get-new-news)
    (gnus-demon-scan-mail)
    (gnus-demon-scan-news))

  (defun jmi/update-notmuch ()
    (if (not (jmi/get-notmuch-proc))
        (let ((dummy (when (get-buffer "*notmuch new*")
                       (kill-buffer "*notmuch new*")))
              (proc  (start-process "notmuch"
                                    "*notmuch new*"
                                    "notmuch" "new" "-q")))
          (set-process-sentinel proc 'jmi/callback-on-notmuch-update))))

  (defun jmi/gnus-setup-inbox-all ()
    "Create persistent inbox.all nnselect group if it doesn't exist.
Idempotent: safe to run on every Gnus startup."
    (unless (gnus-group-entry "nnselect:inbox.all")
      (gnus-group-make-group
       "inbox.all"
       '(nnselect "nnselect")
       nil
       (list
        (cons 'nnselect-specs
              (list
               (cons 'nnselect-function 'gnus-search-run-query)
               (cons 'nnselect-args
                     (list (cons 'search-query-spec '((query . "tag:unread folder:/\\/INBOX$/")))
                           (cons 'search-group-spec
                                 '(("nnmaildir:jmibanez.com" "nnmaildir+jmibanez.com:INBOX")
                                   ("nnmaildir:gmail" "nnmaildir+gmail:INBOX")))))))
        (cons 'nnselect-artlist nil)
        (cons 'nnselect-always-regenerate t)))))

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
    (let* ((message-id (mail-header-id (gnus-summary-article-header)))
           (synthetic-message-id (string-match "<\\(\\w+\\)/\\(\\w+\\)/pull/\\(\\w+\\)\\(/.*\\)?@github.com>"
                                               message-id)))
      (if synthetic-message-id
          (let ((pr-org (match-string 1 message-id))
                (pr-repo (match-string 2 message-id))
                (pr-id   (match-string 3 message-id)))
            (browse-url (format "https://github.com/%s/%s/pull/%s" pr-org pr-repo pr-id)))

        (let ((win (or (gnus-get-buffer-window gnus-article-buffer t)
                       (error "No article window found"))))
          (select-window win)
          (select-frame-set-input-focus (window-frame win))
          (if (fboundp 'w3m-minor-mode)
              (progn
                (w3m-next-anchor n)
                (browse-url (w3m-anchor)))

            (browse-url (forward-button n)))))))

  (defun jmi/gnus-in-home-dir ()
    (interactive)
    (let ((default-directory "~/"))
      (gnus)))

  (defun jmi/gnus-sync-notmuch-on-inbox-all-exit ()
    "Sync notmuch after exiting inbox.all so read state is updated."
    (when (string= gnus-newsgroup-name "nnselect:inbox.all")
      (jmi/update-notmuch)))

  :bind ((:map jmi/my-jump-keys-map
               ("m"       . jmi/gnus-in-home-dir))
         (:map gnus-group-mode-map
               ("/ /"   . jmi/gnus-mail-search-all-inboxes)
               ("/ j"   . jmi/gnus-mail-search-jmibanez)
               ("/ g"   . jmi/gnus-mail-search-gmail)
               ("/ a"   . jmi/gnus-mail-search-archive-only)
               ("/ A"   . jmi/gnus-mail-search-all))
         (:map gnus-summary-mode-map
               ("TAB"     . jmi/gnus-summary-forward-link)
               ("C-<tab>" . jmi/gnus-summary-browse-link-forward))
         (:map gnus-article-mode-map
               ("TAB"     . jmi/gnus-summary-forward-link)))

  :hook ((gnus-started       . jmi/gnus-setup-inbox-all)
         (gnus-summary-exit  . jmi/gnus-sync-notmuch-on-inbox-all-exit))

  :ensure-system-package (msmtp)

  :ensure nil
  :defer t)

(use-package htmlize
  :defer t)

(use-package org-msg
  :defer t

  :config
  (setopt mail-user-agent 'gnus-user-agent)
  (setopt org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
          org-msg-startup "hidestars indent inlineimages"
          org-msg-greeting-fmt "\nHi%s,\n\n"
          org-msg-recipient-names '(("jm@jmibanez.com"  . "JM"))
          org-msg-default-alternatives '((new             . (text html))
                                         (reply-to-html   . (text html))
                                         (reply-to-text   . (text)))
          org-msg-convert-citation t
          org-msg-signature "

Regards,

#+begin_signature

JM Ibañez

https://jmibanez.com/
https://instagram.com/jmibanez
#+end_signature")

  :hook ((gnus-startup  . org-msg-mode))
  :after (org gnus))

(use-package mbsync
  :defer t
  :config
  (defun jmi/do-mail-sync ()
    (interactive)
    (message "Syncing mail...")
    (mbsync))

  ;; Suppress mbsync.el's penchant for switching to the
  ;; mbsync log buffer during errors, which just ruins my frame layout
  (jmi/suppress-buffer-switch-message-instead 'mbsync-process-filter
                                              "mbsync had errors, see buffer *mbsync* for details")
  (jmi/suppress-buffer-switch-message-instead 'mbsync-sentinel
                                              "mbsync had errors, see buffer *mbsync* for details")

  ;; Configure Gnus to poll mbsync periodically (every 5 mins) for
  ;; mail
  (gnus-demon-add-handler 'mbsync 5 nil)

  :ensure-system-package (mbsync . isync)

  :autoload (jmi/do-mail-sync jmi-scan-mail-and-news)

  :hook ((mbsync-exit       .  jmi/update-notmuch)
         (gnus-startup      .  gnus-demon-init))
  :after (gnus)

  :bind ((:map jmi/my-jump-keys-map
               ("f f"      . jmi/do-mail-sync))))

(use-package alert
  :defer t
  :config
  (setq alert-default-style 'osx-notifier))

(use-package gnus-desktop-notify
  :defer t
  :config
  (setopt gnus-desktop-notify-format              "%n: %G"
          gnus-desktop-notify-uncollapsed-levels  nil
          gnus-desktop-notify-behavior            'gnus-desktop-notify-multi)

  :hook ((gnus-started   . gnus-desktop-notify-mode)
         (gnus-exit-gnus . gnus-desktop-notify-mode))
  :after gnus)

(use-package gnus-topic
  :defer t
  :hook
  ((gnus-group-mode               .  gnus-topic-mode))

  :ensure nil
  :after gnus)


;;; communication.init.el ends here
