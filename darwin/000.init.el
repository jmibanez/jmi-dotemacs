;;; 000.init.el -- Darwin-specific (i.e. macOS) config

;;; Commentary:
;;; This contains macOS-specific configuration

;;; Code:

;; Set keys
(setq mac-command-modifier 'super)
(setq mac-option-modifier '(:mouse alt :function meta :ordinary meta))

;; We mark that we've provided platform-specific initialization
(provide 'jmi-init-platform)

;;; 000.init.el ends here
