;; .gnus.el -- Gnus startup
;;

;; Some per-group overrides. We set a large nov-cache-size to speed
;; up opening some groups where we expect to have a large amount of
;; messages. Also, don't discourage HTML when viewing tickets and
;; CRs
(setq gnus-parameters
      '(("INBOX"                           (nov-cache-size . 5000))
        ("INBOX\\.Notifications\\.*"       (nov-cache-size . 7000))
        ("INBOX\\.Notifications\\.CRs"     (mm-discouraged-alternatives nil))
        ("INBOX\\.Notifications\\.Tickets" (mm-discouraged-alternatives nil))))

(setq gnus-nov-is-evil t)

(setq message-send-mail-function           'sendmail-send-it
      send-mail-function                   'sendmail-send-it
      sendmail-program                     "~/scripts/sendmail-tunnel.sh"
      imap-enable-exchange-bug-workaround  t)



;; Primary: Work email

;; NB: We're using Maildirs + mbsync to decouple ourselves from the
;; vagaries of nnimap and the network/VPN.
(setq gnus-select-method
      '(nnmaildir "amazon"
                  (directory             "~/Maildir/amazon")))

;; Search via mairix
;; NB: We need to require nnmairix to load it and have its keybindings active
(require 'nnmairix)
(setq gnus-secondary-select-methods
      '((nnmaildir "mairix"
                   (directory     "~/.nnmairix"))
        (nnml      "archive"
                   (nnml-directory             "~/Mail.archive")
                   ;; Don't expire messages in the archive!
                   (nnml-inhibit-expiry        t))))

;; Secondary: Personal GMail accounts
;; (setq gnus-secondary-select-methods
;;       '((nnimap "gmail"
;;                 (nnimap-address     "imap.gmail.com")
;;                 (nnimap-stream      ssl)
;;                 (nnimap-server-port "imaps")
;;                 (nnmail-expiry-target ))))

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
(setq mm-html-inhibit-images nil)
(setq mm-html-blocked-images nil)
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

;; Ignore some "noise" words that frequently come up in mail; these
;; will often be in any email that I read or not read, that
;; fuzzy/adaptive scoring based on these words just breaks things
(setq gnus-ignored-adaptive-words
      '(;; Ignore months
        "january"
        "february"
        "march"
        "april"
        "may"
        "june"
        "july"
        "august"
        "september"
        "october"
        "november"
        "december"))


;; Threads and thread handling
;; Thread display chars
(setq gnus-sum-thread-tree-root             "="
      gnus-sum-thread-tree-false-root       "< "
      gnus-sum-thread-tree-single-indent    " "
      gnus-sum-thread-tree-indent           "  "
      gnus-sum-thread-tree-leaf-with-other  "+-> "
      gnus-sum-thread-tree-single-leaf      "`-> "
      gnus-sum-thread-tree-vertical         "| ")

;; Don't fold headers, since we have a fairly wide window anyway
(setq gnus-treat-fold-headers nil)
(setq gnus-treat-unfold-headers 'head)

;; Sort threads by score before date (descending, recent first)
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-score
        (not gnus-thread-sort-by-date)
        gnus-thread-sort-by-total-score))

;; Fetch some older messages in the thread, for context
(setq gnus-fetch-old-headers 'some)

;; Posting styles
(setq gnus-posting-styles
      '((".*"
         (name            "JM Ibanez")
         (address         "jm@jmibanez.com")
         (signature-file  "~/.signature"))))

