;;; 001-theme.init.el -- Theming-specific functions and config

;;; Commentary:
;;; This configures the modeline (via powerline, spaceline, and mode
;;; icons) and adds a few functions to toggle the Solarized Emacs
;;; theme between light and dark

;;; Code:

;; Set default fonts
(if (fboundp 'mac-auto-operator-composition-mode)

    (mac-auto-operator-composition-mode)

  ;; If not running railwaycat/emacs-mac: Use ligature.el
  (progn
    ;; ligature.el isn't in MELPA yet, so pull it in from our local elisp load path
    (use-package ligature
      :config
      (ligature-set-ligatures 't '("www"))

      ;; Enable ligatures in programming modes
      (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                           ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                           "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                           "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                           "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                           "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                           "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                           "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                           "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                           "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

      (global-ligature-mode 't)

      :load-path "~/elisp/ligature.el")))

(set-frame-font "Fira Code 11" nil t)

;; Load theme
(use-package flucui-themes
  :init
  (setq frame-background-mode 'dark)

  :config
  (load-theme 'flucui-dark t)
  (setq jmi/selected-theme 'flucui-dark))

;; Functions to toggle between light and dark
(defun jmi/apply-theme-dark ()
  (interactive)
  (set-frame-parameter nil 'background-mode 'dark)
  (enable-theme jmi/selected-theme))

(defun jmi/apply-theme-light ()
  (interactive)
  (set-frame-parameter nil 'background-mode 'light)
  (enable-theme jmi/selected-theme))

;; Modeline config
(use-package powerline
  :init
  (setq powerline-default-separator 'butt))

(use-package spaceline
  :config
  (spaceline-helm-mode)

  :after helm)

(use-package all-the-icons
  :if
  (display-graphic-p)
  :after spaceline)

(use-package spaceline-all-the-icons
  :config
  (setq spaceline-all-the-icons-icon-set-eyebrowse-slot 'solid)
  (spaceline-toggle-all-the-icons-buffer-path-off)
  (spaceline-toggle-all-the-icons-buffer-size-off)
  (spaceline-toggle-all-the-icons-flycheck-status-info-off)
  (spaceline-all-the-icons-theme)

  :if
  (display-graphic-p)

  :after (spaceline all-the-icons))

;; Pop-up windows when display-buffer
(setq pop-up-windows t)

;; Indicate buffer boundaries
(setq-default indicate-buffer-boundaries '((top . left) (t . right)))


;;; 001-theme.init.el ends here
