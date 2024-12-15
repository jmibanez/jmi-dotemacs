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

(use-package anti-zenburn-theme
  :config
  (load-theme 'anti-zenburn :no-confirm)

  (setq jmi/selected-theme 'anti-zenburn)

  (set-face-attribute 'default nil :font "Berkeley Mono-14")
  (set-frame-font "Berkeley Mono-14" nil t t)

  :custom-face
  ;; Remap ansi-color-<color> faces
  (ansi-color-black  ((t (:foreground "#232333"))))
  (ansi-color-red ((t (:foreground "#6c1f1c"))))
  (ansi-color-green ((t (:foreground "#23733c"))))
  (ansi-color-yellow ((t (:foreground "#732f2c"))))
  (ansi-color-blue ((t (:foreground "#0f2050"))))
  (ansi-color-magenta ((t (:foreground "#806080"))))
  (ansi-color-cyan ((t (:foreground "#336c6c"))))
  (ansi-color-gray ((t (:foreground "#c0c0c0"))))

  ;; Also fix company-preview so it uses a light grey foreground, default black is unreadable
  (company-preview ((t (:background "#603a60" :foreground "#FFFFFF")))))

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

;; Pop-up windows when display-buffer
(setq pop-up-windows t)

;; Indicate buffer boundaries
(setq-default indicate-buffer-boundaries '((t . right)))

;;; 001-theme.init.el ends here
