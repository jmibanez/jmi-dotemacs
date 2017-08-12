;; From this post:
;; http://cgwalters.livejournal.com/20363.html
;; The default for "uniquifying" buffer names sucks
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))
