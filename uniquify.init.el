;;; uniquify.init.el -- Unique buffer names  -*- lexical-binding: t; -*-

;;; Commentary:

;;; We want to have easy to distinguish buffer names, especially when
;;; editing files with the same name in different directories
;;; (e.g. ~/.emacs.init/000.init.el, ~/.emacs.init/darwin/000.init.el);
;;; from this post: http://cgwalters.livejournal.com/20363.html
;;; the default for "uniquifying" buffer names sucks, so change it

;;; Code:

(use-package uniquify
  :ensure nil ;; System package
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;;; uniquify.init.el ends here
