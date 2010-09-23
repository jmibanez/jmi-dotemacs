;; Magit - Emacs interface to Git
(require 'magit)

;; Point Magit to MacPorts git
(setq magit-git-executable "/opt/local/bin/git")

;; Set default magit dirs
(setq magit-repo-dirs
      '("~/projects/sinefunc"
        "~/projects/personal"
        "~/projects/skunk"
        "~/projects/codeflux"))
