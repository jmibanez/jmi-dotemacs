(require 'erc)
(require 'erc-match)

(setq erc-keywords '("sm56" "motorola" "git-svn" "orangeandbronze" "jm" "jmibanez"))

(setq erc-server "irc.freenode.net"
      erc-port 6667
      erc-nick "jmibanez"
      erc-user-full-name user-full-name)

(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#git" "#ubuntu" "#ubuntu-ph" "#ubuntu-offtopic")))

(require 'erc-fill)
(erc-fill-mode t)

(setq erc-log-insert-log-on-open nil)
(setq erc-log-channels t)
(setq erc-log-channels-directory "~/Documents/misc/erc-logs/")
(setq erc-save-buffer-on-part t)
(setq erc-hide-timestamps nil)
;; (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
;;   (save-some-buffers t (lambda () (when (and (eq major-mode 'erc-mode)
;;                                              (not (null buffer-file-name)))))))

(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
(add-hook 'erc-mode-hook '(lambda () (when (not (featurep 'xemacs))
                                       (set (make-variable-buffer-local
                                             'coding-system-for-write)
                                            'emacs-mule))))

;; Minimal distraction
(setq erc-current-nick-highlight-type 'nick)

(setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))
(setq erc-track-use-faces t)
(setq erc-track-faces-priority-list
      '(erc-current-nick-face erc-keyword-face))
(setq erc-track-priority-faces-only 'all)
