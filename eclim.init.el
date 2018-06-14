;;; eclim.init.el -- Eclim config

;;; Commentary:

;;; Code:
(use-package eclim
  :init
  (setq jmi/eclipse-dir "~/apps/eclipse/Eclipse.app/Contents/Eclipse/")

  (setq eclimd-executable (concat jmi/eclipse-dir "eclimd"))
  (setq eclim-executable (concat jmi/eclipse-dir "eclim"))
  (setq eclim-eclipse-dirs jmi/eclipse-dir)

  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 1)

  (setq eclim-auto-save t)
  ;;  (add-to-list 'eclim--file-coding-system-mapping '("iso-latin-1-dos" . "ISO-8859-1"))

  :config
  (help-at-pt-set-timer)
  (global-eclim-mode))


;; Local help, primarily for eclim
(use-package ac-emacs-eclim-source
  :config
  (ac-emacs-eclim-config))

;;; eclim.init.el ends here
