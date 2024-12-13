;;; zzz.init.el -- Things that *must* be configured last

;;; Commentary:

;;; This performs configuration on things that must be loaded last,
;;; due to some quirks, or because we need a specific way to ensure
;;; other stuff are loaded first (e.g. ensuring things that
;;; emacs-startup-hook depends on are already configured/loaded).

;;; Code:

(provide 'jmi/all-config-loaded)
;;; zzz.init.el ends here
