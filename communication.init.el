;;; communication.init.el -- Chat and comms

;;; Commentary:
;;; RSS feed readers, IRC, in-Emacs Twitter support, etcetera.

;;; Code:

(use-package elfeed
  :init
  (setq elfeed-feeds
        '(("https://blog.jmibanez.com/feed.xml" blog)
          ("https://xkcd.com/atom.xml" comics)
          ("https://www.gpf-comics.com/rss/main_comic_rss.xml" comics)
          ("http://leancrew.com/all-this/feed/" blog)
          ("https://daringfireball.net/feeds/main" blog apple)
          ("http://feeds.arstechnica.com/arstechnica/index" news)
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
          ("http://hownow.brownpau.com/feed" blog)))

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
               ("S-<f7>"  .  jmi/elfeed-show-bookmark-entry)))

  :after jmi-keybindings)


(use-package circe)

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
  :ensure t)

(use-package xwwp
  :config

  (setq xwwp-search-prefix "https://duckduckgo.com/?q="))


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
  (setq gnus-init-file (concat jmi/my-emacs-init-path ".gnus.el"))

  ;; HTML email composition via Markdown
  (defun jmi/mimedown ()
    (interactive)
    (save-excursion
      (message-goto-body)
      (shell-command-on-region (point) (point-max)
                               "/Users/jabz/scripts/mimedown.sh" nil t)))

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

  :bind ((:map jmi/my-jump-keys-map
               ("m"       . gnus))
         (:map gnus-summary-mode-map
               ("TAB"     . jmi/gnus-summary-forward-link)
               ("C-<tab>" . jmi/gnus-summary-browse-link-forward))
         (:map gnus-article-mode-map
               ("TAB"     . jmi/gnus-summary-forward-link)))

  :hook
  ((message-send             .  jmi/mimedown))

  :ensure nil
  :after (jmi-keybindings))

(use-package mbsync
  :config
  (defun jmi/scan-mail-and-news ()
    ;; Update mairix groups -- mairix must have been configured already...
    (nnmairix-update-database)
    (gnus-demon-scan-mail)
    (gnus-demon-scan-news))

  (defun jmi/do-mail-sync ()
    (interactive)
    (message "Syncing mail...")
    (mbsync))

  ;; Suppress mbsync.el's penchant for switching to the
  ;; mbsync log buffer during errors, which just ruins my frame layout
  (defun jmi/suppress-switch-to-buffer-other-window (buf)
    (message "mbsync had errors, see buffer *mbsync* for details"))

  (defun jmi/mbsync-advice-fn (orig &rest args)
    (cl-letf (((symbol-function 'switch-to-buffer-other-window)
           #'jmi/suppress-switch-to-buffer-other-window))
      (apply orig args)))

  (advice-add 'mbsync-process-filter :around
              #'jmi/mbsync-advice-fn)
  (advice-add 'mbsync-sentinel :around
              #'jmi/mbsync-advice-fn)

  ;; Configure Gnus to poll mbsync periodically (every 5 mins) for
  ;; mail
  (gnus-demon-add-handler 'mbsync 5 nil)

  :ensure-system-package isync

  :autoload (jmi/do-mail-sync jmi-scan-mail-and-news)

  :hook ((mbsync-exit       .  jmi/scan-mail-and-news)
         (gnus-startup      .  gnus-demon-init))
  :after (jmi-keybindings gnus))

(use-package alert
  :config
  (setq alert-default-style 'notifier)

  ;; We need to replace the -appIcon switch with -contentImage, as
  ;; terminal-notifier can't replace the notification icon
  (defun alert-notifier-notify (info)
    (if alert-notifier-command
        (let ((args
               (list "-title"   (alert-encode-string (plist-get info :title))
                     "-contentImage" (or (plist-get info :icon) alert-notifier-default-icon)
                     "-message" (alert-encode-string (plist-get info :message)))))
          (apply #'call-process alert-notifier-command nil nil nil args))
      (alert-message-notify info)))

  :ensure-system-package terminal-notifier

  :demand t)

(use-package gnus-desktop-notify
  :config
  (setq gnus-desktop-notify-format "%n: %G")
  (setq gnus-desktop-notify-uncollapsed-levels nil)
  (setq gnus-desktop-notify-behavior 'gnus-desktop-notify-multi)

  ;; Replace gnus-desktop-notify-alert with one that provides an icon specifically for Gnus
  (defun gnus-desktop-notify-alert (body)
    "Replacement for actual gnus-desktop-notify-alert which provides an icon"
    (alert body
           :title gnus-desktop-notify-send-subject))

  (gnus-desktop-notify-mode)

  :after gnus)

(use-package gnus-topic
  :hook
  ((gnus-group-mode               .  gnus-topic-mode))

  :ensure nil
  :after gnus)


;;; communication.init.el ends here
