;; Custom / Customize
(setq custom-file "~/.emacs.init/custom.init.el")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-epiphany))
 '(blink-cursor-mode t)
 '(browse-url-firefox-new-window-is-tab nil)
 '(browse-url-new-window-flag t)
 '(canlock-password "50fb069cd36ade85200c3c4f79a8e38ccb9db285")
 '(cperl-electric-keywords t)
 '(cperl-hairy t)
 '(ecb-directories-menu-user-extension-function (quote ignore))
 '(ecb-history-menu-user-extension-function (quote ignore))
 '(ecb-layout-name "top1")
 '(ecb-layout-window-sizes (quote (("jmitop" (0.3974358974358974 . 0.2857142857142857) (0.6794871794871795 . 0.2857142857142857)))))
 '(ecb-methods-menu-user-extension-function (quote ignore))
 '(ecb-options-version "2.32")
 '(ecb-other-window-behavior (quote smart))
 '(ecb-sources-menu-user-extension-function (quote ignore))
 '(ecb-tip-of-the-day nil)
 '(emacs-wiki-maintainer "mailto:jmibanez@gmail.com")
 '(emacs-wiki-publishing-directory "~/public_html/wiki")
 '(fortune-file "/usr/share/games/fortunes/")
 '(global-semantic-folding-mode t t)
 '(global-semantic-idle-scheduler-mode nil nil (semantic-idle))
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote left))
 '(initial-buffer-choice t)
 '(jde-mvn-default-compiler-source "1.5")
 '(jde-mvn-default-compiler-target "1.5")
 '(lj-default-username "jmibanez")
 '(lj-fill-function (quote lj-fill-by-paragraph))
 '(mm-url-use-external t)
 '(ns-pop-up-frames nil)
 '(scheme-mit-dialect nil)
 '(scheme-program-name "mzscheme")
 '(sql-oracle-program "~/scripts/ssh-sqlplus.sh")
 '(transient-mark-mode t)
 '(w3m-command "/opt/local/bin/w3m")
 '(w3m-default-display-inline-images t)
 '(w3m-tab-width 4)
 '(w3m-use-cookies t)
 '(x-select-enable-clipboard t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(newsticker-feed-face ((t (:foreground "misty rose" :weight bold :height 1.5 :family "helvetica"))))
 '(org-hide ((((background dark)) (:foreground "#0a0a38")))))

;;--(custom-set-faces
;;--  ;; custom-set-faces was added by Custom.
;;--  ;; If you edit it by hand, you could mess it up, so be careful.
;;--  ;; Your init file should contain only one such instance.
;;--  ;; If there is more than one, they won't work right.
;;-- '(flymake-errline ((((class color)) (:underline "OrangeRed"))))
;;-- '(flymake-warnline ((((class color)) (:underline "yellow"))))
;;-- '(mode-line ((t (:background "grey40" :foreground "black" :box (:line-width -1 :style released-button)))))
;;-- '(mode-line-inactive ((t (:inherit mode-line :background "grey20"))))
;;-- '(org-hide ((((background dark)) (:foreground "#0a0a38")))))


