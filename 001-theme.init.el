;;; 001-theme.init.el -- Theming-specific functions and config

;;; Commentary:
;;; This configures the modeline (via powerline, spaceline, and mode
;;; icons) and adds a few functions to toggle the Solarized Emacs
;;; theme between light and dark

;;; Code:

;; Set default fonts
(if (fboundp 'mac-auto-operator-composition-mode)
    ;; emacs-mac only; see next for emacs-plus config
    (mac-auto-operator-composition-mode) ;; Needed for ligatures in Berkeley Mono/Fira Code/etc.

  (use-package ligature
    :config
    (ligature-set-ligatures 't '("www"))

    ;; Enable ligatures in programming modes
    (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                         ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                         "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                         "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                         "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                         "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                         "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                         "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                         ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                         "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                         "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                         "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                         "\\\\" "://"))
    (global-ligature-mode 't)))

(use-package modus-themes
  :defer nil

  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-org-blocks 'gray-background
        modus-themes-common-palette-overrides '((comment blue-faint)
                                                (border-mode-line-active unspecified)
                                                (border-mode-line-inactive unspecified)
                                                (bg-mode-line-active bg-blue-subtle)
                                                (fg-mode-line-active fg-main))
        modus-themes-headings '((1 . (variable-pitch 1.5))
                                (2 . (1.3))
                                (agenda-date . (1.3))
                                (agenda-structure . (variable-pitch light 1.8))
                                (t . (1.1))))

  (defun jmi/modus-theme-support-faces (_)
    (modus-themes-with-colors
      (custom-set-faces
       ;; Ensure eshell-info-banner text color is the same as the theme background color (for contrast against the normal
       ;; progress bar values)
       `(eshell-info-banner-background-face ((,c :foreground ,bg-dim)))

       ;; Fix epe-git-dir-face to be readable against white (default yellow is too bright)
       `(epe-git-dir-face ((,c :foreground ,fg-dim))))))
  (add-hook 'enable-theme-functions #'jmi/modus-theme-support-faces)

  :config
  (load-theme 'modus-vivendi-tinted :no-confirm)
  (setq jmi/selected-theme 'modus-vivendi-tinted)

  (set-face-attribute 'default nil :font "Berkeley Mono-14")
  (set-frame-font "Berkeley Mono-14" nil t t))

(use-package all-the-icons
  :config
  (push
   '(groovy-mode  all-the-icons-fileicon "groovy" :face all-the-icons-green)
   all-the-icons-mode-icon-alist)
  :if
  (display-graphic-p))

(use-package mood-line
  :config
  ;; Use Fira Code-compatible glyphs, which are also compatible with
  ;; Berkeley Mono
  (require 'mood-line-segment-vc)

  (defconst jmi/mood-line-glyphs
    '((:checker-info . ?↳)
      (:checker-issues . ?→)
      (:vc-good . ?-)
      (:checker-checking . ?⟳)
      (:checker-errored . ?x)
      (:checker-interrupted . ?=)

      (:vc-added . ?+)
      (:vc-needs-merge . ?⟷)
      (:vc-needs-update . ?↓)
      (:vc-conflict . ?x)
      (:vc-good . ?-)

      (:buffer-narrowed . ?◢)
      (:buffer-modified . ?◊)
      (:buffer-read-only . ?■)

      (:frame-client . ?)

      (:count-separator . ?×)))

  (defun jmi/mood-line-segment-major-mode ()
    (let ((icon (all-the-icons-icon-for-mode major-mode
                                             :v-adjust 0
                                             :height 0.8)))
      (if (eq icon major-mode)
          mode-name
        (propertize icon
                    'help-echo mode-name))))


  (setq mood-line-glyph-alist jmi/mood-line-glyphs)
  (setq mood-line-format
        (mood-line-defformat
         :left
         (((mood-line-segment-modal)            . " ")
          ((mood-line-segment-project)          . "")
          ((or (mood-line-segment-buffer-status)
               (mood-line-segment-client)
               " ")                             . "")
          ((mood-line-segment-buffer-name)            . "  ")
          ((mood-line-segment-anzu)                   . "  ")
          ((mood-line-segment-multiple-cursors)       . "  ")
          ((mood-line-segment-cursor-position)        . " ")
          (mood-line-segment-scroll))
         :right
         (((mood-line-segment-vc)         . "  ")
          ((jmi/mood-line-segment-major-mode) . "  ")
          ((mood-line-segment-misc-info)  . "  ")
          ((mood-line-segment-checker)    . "  ")
          ((mood-line-segment-process)    . "  "))))

  (mood-line-mode)

  :after all-the-icons)

(use-package anzu
  :config
  (global-anzu-mode))

;; Pop-up windows when display-buffer
(setq pop-up-windows t)

;; Indicate buffer boundaries
(setq-default indicate-buffer-boundaries '((t . right)))

;; Startup screen
(use-package jmi-startup-screen
  :ensure nil
  :demand t
  :hook (emacs-startup . jmi/startup-screen))


;;; 001-theme.init.el ends here
