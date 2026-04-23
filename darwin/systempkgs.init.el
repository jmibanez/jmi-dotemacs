;;; systempkgs.init.el -- macOS system packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Ensures required system packages (fonts, tools) are installed via Homebrew.

;;; Code:

;; Ensure fontconfig is available for font detection
(unless (executable-find "fc-list")
  (system-packages-install "fontconfig"))

;; Font: Noto Sans Symbols 2 (Unicode symbol coverage beyond Apple Symbols)
(when (executable-find "fc-list")
  (unless (with-temp-buffer
            (call-process "fc-list" nil t nil ":family=Noto Sans Symbols 2")
            (> (buffer-size) 0))
    (system-packages-install "font-noto-sans-symbols-2")))

;;; systempkgs.init.el ends here