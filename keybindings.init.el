;;; keybindings.init.el -- Keybindings and keybinding suport

;;; Commentary:

;;; Keybindings-only config, This adds a custom keymap for custom
;;; functions; by default, I've bound it to <f8>. Also provides a few
;;; custom functions to DWIM (e.g. for scope-based find-file)

;;; Code:

(use-package bind-key
  :ensure nil

  :config
  (defun jmi/in-org-roam-related-buffer-p ()
    (or (and (fboundp 'jmi/org-roam-buffer-p)
             (jmi/org-roam-buffer-p))
        (eq major-mode 'org-agenda-mode)))

  (defun jmi/projectile-find-file-or-org-roam-node-find-dwim ()
    "Either do projectile-find-file or or do org-roam-find; DWIM."
    (interactive)

    (if (not (fboundp 'projectile-find-file))
        (error "Projectile is not loaded.")

      (if (jmi/in-org-roam-related-buffer-p)
          (org-roam-node-find)

        ;; Else, default to projectile-find-file
        (projectile-find-file-dwim))))

  ;; On keyboards which don't have Super (e.g. my Model M) bind C-& to
  ;; event-apply-super-modifier so I can reach my bindings that use Super
  (define-key function-key-map (kbd "C-&") 'event-apply-super-modifier)

  :bind
  (("s-t"   . jmi/projectile-find-file-or-org-roam-node-find-dwim)
   (:map jmi/my-jump-keys-map
         ("c c"      . projectile-compile-project)
         ("f p"      . jmi/toggle-http-proxy)
         ("f f"      . jmi/do-mail-sync))))

;;; keybindings.init.el ends here
