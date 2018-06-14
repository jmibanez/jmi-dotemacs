;;; magit.init.el -- Config for magit

;;; Commentary:

;;; Code:

;; Magit - Emacs interface to Git
(use-package magit
  :init
  ;; Point Magit to locally installed git (not system)
  (setq magit-git-executable "/usr/local/bin/git")

  ;; Set default magit dirs
  (setq magit-repo-dirs
        '("~/projects/personal"
          "~/projects/skunk"
          "~/projects/freelance"
          "~/projects/codeflux"))
  (setq magit-use-overlays nil))

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t))

(use-package github-notifier
  :init
  ;; github-notifier
  (setq github-notifier-token "af0c5ea4b683f6fe728c1729430915344528411f"))


(use-package magit-gh-pulls
  :init
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package fullframe
  :config
  (fullframe magit-status magit-mode-quit-window nil))

;;; magit.init.el ends here
