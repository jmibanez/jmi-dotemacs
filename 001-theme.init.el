;;; 001-theme.init.el -- Theming-specific functions and config

;;; Commentary:
;;; This configures the modeline (via powerline, spaceline, and mode
;;; icons) and adds a few functions to toggle the Solarized Emacs
;;; theme between light and dark

;;; Code:

;; Set default fonts
(if (fboundp 'mac-auto-operator-composition-mode)
    ;; emacs-mac only; see next for emacs-plus config
    (mac-auto-operator-composition-mode) ;; Needed for ligatures in Fira Code

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

    :load-path "~/elisp/ligature.el"))

;; Load theme
(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs t
        modus-themes-syntax '(yellow-comments)
        modus-themes-mode-line '(accented moody borderless)
        modus-themes-org-blocks 'gray-background
        modus-themes-markup '(background italic))

  :config
  (load-theme 'modus-vivendi :no-confirm)
  (setq jmi/selected-theme 'modus-vivendi)

  (set-face-attribute 'default nil :font "Berkeley Mono-14")
  (set-frame-font "Berkeley Mono-14" nil t t))


(use-package all-the-icons
  :if
  (display-graphic-p))

(use-package mood-line
  :config
  (mood-line-mode))

;; Pop-up windows when display-buffer
(setq pop-up-windows t)

;; Indicate buffer boundaries
(setq-default indicate-buffer-boundaries '((top . left) (t . right)))

;;; 001-theme.init.el ends here
