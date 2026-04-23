;;; systempkgs.init.el -- Linux system packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Ensures required system packages (fonts, tools) are installed via the system package manager.

;;; Code:

;; Font: Noto Sans Symbols 2 (Unicode symbol coverage beyond Apple Symbols)
(unless (with-temp-buffer
          (call-process "fc-list" nil t nil ":family=Noto Sans Symbols 2")
          (> (buffer-size) 0))
  (system-packages-install "fonts-noto-extra"))

;;; systempkgs.init.el ends here