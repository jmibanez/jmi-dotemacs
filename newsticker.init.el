;; Newsticker config

;; Format/Layout
(setq newsticker-heading-format "%t %d %s")
(setq newsticker-item-format "%d :: %t")
(setq newsticker-desc-format "%c

")

(setq newsticker-hide-old-items-in-newsticker-buffer t)
(setq newsticker-html-renderer 'w3m-region)
(setq newsticker-show-descriptions-of-new-items t)


;; News feeds
(setq newsticker-url-list
      '(("Sacha Chua" "http://sachachua.com/notebook/wiki/blog.rdf" nil 86400 nil)
        ("The Fishbowl" "http://fishbowl.pastiche.org/index.rdf" nil 86400 nil)
        ("Sam Newman's blog (magpiebrain)" "http://feeds.feedburner.com/Magpiebrain" nil 86400 nil)
        ("Groklaw" "http://www.groklaw.net/backend/GrokLaw.rdf" nil 86400 nil)
        ("Dare Obasanjo" "http://feeds.feedburner.com/Carnage4life" nil 3600 nil)
        ("Slashdot" "http://rss.slashdot.org/Slashdot/slashdot" nil 86400 nil)
        ("Planet GNOME" "http://planet.gnome.org/rss20.xml" nil 3600 nil)
        ("Planet Ubuntu" "http://planet.ubuntu.com/rss20.xml" nil 3600 nil)))

(setq newsticker-use-full-width nil)


