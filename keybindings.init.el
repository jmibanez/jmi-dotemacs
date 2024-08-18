;;; keybindings.init.el -- Keybindings and keybinding suport

;;; Commentary:

;;; This adds a custom keymap for custom functions; by default, I've
;;; bound it to <f8>.  This supersedes my old keybindings.el config

;;; Code:


;; To avoid any weirdness, we don't use use-package here --
;; use-package depends on bind-key, and we're using bind-key pretty
;; directly anyway
(require 'bind-key)

;; User function: Toggle http_proxy in process-environment. Used so I
;; can go online etc. via a tunneled connection.

(defun jmi/toggle-http-proxy ()
  "Toggle the value/existence of http_proxy and the TSOCKS proxy."
  (interactive)
  (if (getenv "http_proxy")
      (setenv "http_proxy" nil)
    (setenv "http_proxy" "http://localhost:8888"))
  (jmi/do-toggle-tsocks)
  (message (let ((proxy (getenv "http_proxy")))
             (cond (proxy)
                   ("ENV: Unset proxy")))))

;; FIXME: Currently broken -- need to rewrite to work on either macOS or Linux
(defun jmi/do-toggle-tsocks ()
  "Toggle LD_PRELOAD for tsocks."
  (if (and t;; (getenv "LD_PRELOAD") ;; Stupid hack to get LD_PRELOAD synched with http_proxy
           (getenv "http_proxy"))
      (setenv "LD_PRELOAD" "/usr/lib/libtsocks.so")
    (setenv "LD_PRELOAD" nil)))


(defun jmi/projectile-find-file-or-org-roam-node-find-dwim ()
  "Either do projectile-find-file or or do org-roam-find; DWIM."
  (interactive)

  (if (not (fboundp 'projectile-find-file))
      (error "Projectile is not loaded.")

    (if (and (fboundp 'jmi/org-roam-buffer-p)
             (eq major-mode 'org-mode)
             (jmi/org-roam-buffer-p))
        (org-roam-node-find)

      ;; Else, default to projectile-find-file
      (projectile-find-file-dwim))))

;; Our shortcut map
(bind-keys :prefix-map jmi/my-jump-keys-map
           :prefix "<f8>"

           ("c c"      . compile)
           ("f p"      . jmi/toggle-http-proxy)
           ("f f"      . jmi/do-mail-sync))

(global-set-key [(super t)] #'jmi/projectile-find-file-or-org-roam-node-find-dwim)

(provide 'jmi-keybindings)

;;; keybindings.init.el ends here
