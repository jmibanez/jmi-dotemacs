;;; jmi-startup-screen.el -- Startup screen hook

;;; Commentary:

;;; Provides a function that we can put in the emacs-startup-hook. Use
;;; with `use-package` so that the startup hook can depend on specific
;;; packages being loaded first (e.g. org-mode)

;;; Code:

(defun jmi/startup-screen ()
  "Function to be used as emacs-startup-hook for startup screen."
  (with-eval-after-load 'jmi/all-config-loaded
    (require 'projectile)
    (require 'org)
    (require 'org-roam)
    (require 'eshell)
    (let ((buffer-agenda "*Org Agenda*")
          (buffer-eshell "*eshell*"))
      (org-todo-list)
      (eshell)
      (switch-to-buffer (get-buffer buffer-agenda))
      (delete-other-windows)
      (split-window-vertically)
      (other-window 1)
      (switch-to-buffer (get-buffer buffer-eshell)))))

(provide 'jmi-startup-screen)

;;; jmi-startup-screen.el ends here
