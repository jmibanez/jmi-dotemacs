;;; flycheck.init.el -- Flycheck

;;; Commentary:
;;; All flycheck config go here

;;; Code:

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)

  :config
  (declare-function python-shell-calculate-exec-path "python")

  (defun flycheck-virtualenv-set-python-executables ()
    "Set Python executables for the current buffer."
    (let ((exec-path (python-shell-calculate-exec-path)))
      (setq-local flycheck-python-pylint-executable
                  (executable-find "pylint"))
      (setq-local flycheck-python-flake8-executable
                  (executable-find "flake8"))))

  (defun flycheck-virtualenv-setup ()
    "Setup Flycheck for the current virtualenv."
    (when (derived-mode-p 'python-mode)
      (add-hook 'hack-local-variables-hook
                #'flycheck-virtualenv-set-python-executables 'local)))

  (provide 'flycheck-virtualenv))

;;; flycheck.init.el ends here
