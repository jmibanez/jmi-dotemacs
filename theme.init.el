;;; theme.init.el -- Theming-specific functions and config

;;; Commentary:
;;; This just adds a couple of funs to set the theme for a frame;
;;; actual theme config is set up in the early config (000.init.el)

;;; Code:

(defun jmi/apply-solarized-dark ()
  (interactive)
  (set-frame-parameter nil 'background-mode 'dark)
  (enable-theme 'solarized))

(defun jmi/apply-solarized-light ()
  (interactive)
  (set-frame-parameter nil 'background-mode 'light)
  (enable-theme 'solarized))

;;; theme.init.el ends here
