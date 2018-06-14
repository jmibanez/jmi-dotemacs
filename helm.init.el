;;; helm.init.el -- Helm config

;;; Commentary:
;;; This is pretty bare at the moment; I'll populate this once I get
;;; the hang of using Helm

;;; Code:

(use-package helm-config
  :config
  (global-set-key (kbd "C-c h") 'helm-mini))


;;; helm.init.el ends here
