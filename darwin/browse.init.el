;;; browse.init.el -- Open URL in macOS

;;; Commentary:

;;; Code:
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "/usr/bin/open")

;;; browse.init.el ends here
