;;; custom.init.el -- Custom/Customize dump file

;;; Commentary:
;;; All settings configured via customize will be placed here, before
;;; they are to be moved to their appropriate config files

;;; Code:

;; Point customize to this file
(setq custom-file (concat jmi/my-emacs-init-path "001-custom.init.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ac-html-bootstrap swift-mode projectile helm flycheck magit markdown-mode go-mode cider clojure-mode paredit mode-icons spaceline powerline gnupg tide coffee-mode textmate zenburn-theme yasnippet-bundle yaml-mode use-package underwater-theme twittering-mode thrift tabbar-ruler symon subatomic-theme spaceline-all-the-icons slamhound session scratch-palette scheme-complete rainbow-delimiters pyvenv python-django protobuf-mode projectile-speedbar persp-projectile password-generator org-present oauth2 nyan-mode noflet noctilux-theme magithub magit-gitflow magit-gh-pulls magit-find-file magit-filenotify lua-mode langtool js2-mode jinja2-mode javap-mode helm-themes helm-spotify helm-rails helm-projectile-all helm-projectile helm-package helm-git-files helm-git helm-dash helm-cmd-t helm-chronos groovy-mode go-projectile go-autocomplete github-pullrequest github-notifier git-timemachine gist fullframe fringe-helper flymake-python-pyflakes flycheck-rust flycheck-pyflakes flycheck-pos-tip flycheck-dmd-dub flx fixmee fish-mode firebelly-theme fill-column-indicator fic-mode emojify emms emacs-eclim dockerfile-mode deferred dash-at-point d-mode cyberpunk-theme color-theme-solarized clues-theme clj-refactor circe chicken-scheme bubbleberry-theme bbdb autodisass-java-bytecode ample-zen-theme ample-theme alert afternoon-theme ac-python ac-emacs-eclim ac-cider-compliment ac-cider 2048-game))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight light :height 100 :width normal :foundry "apple" :family "Hack"))))
 '(cursor ((t (:background "Grey" :inverse-video t))))
 '(fixed-pitch ((t (:family "Hack"))))
 '(newsticker-feed-face ((t (:foreground "misty rose" :weight bold :height 1.5 :family "helvetica"))))
 '(org-hide ((((background dark)) (:foreground "Grey15")))))

;;; custom.init.el ends here

