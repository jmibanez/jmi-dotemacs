;;; packages.el -- Packages/Packaging configuration

;;; Commentary:

;;; We set up ELPA configuration here, including adding marmalade as a
;;; repo and listing the packages that we need installed

;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(defvar jmi/packages '(2048-game
                       ac-cider
                       ac-cider-compliment
                       ac-emacs-eclim
                       ac-python
                       alert
                       async
                       auto-complete
                       autodisass-java-bytecode
                       back-button
                       bbdb
                       button-lock
                       chicken-scheme
                       chronos
                       circe
                       cider
                       clj-refactor
                       clojure-mode
                       coffee-mode
                       d-mode
                       dash
                       dash-at-point
                       deferred
                       dockerfile-mode
                       edn
                       emacs-eclim
                       emms
                       emojify
                       epl
                       f
                       fic-mode
                       fill-column-indicator
                       fish-mode
                       fixmee
                       flx
                       flycheck
                       flycheck-dmd-dub
                       flycheck-pos-tip
                       flycheck-pyflakes
                       flycheck-rust
                       flymake-easy
                       flymake-python-pyflakes
                       fringe-helper
                       fullframe
                       gh
                       gist
                       git-commit
                       github-notifier
                       github-pullrequest
                       gntp
                       go-autocomplete
                       go-eldoc
                       go-guru
                       go-mode
                       go-projectile
                       go-rename
                       groovy-mode
                       helm
                       helm-chronos
                       helm-cmd-t
                       helm-dash
                       helm-git
                       helm-git-files
                       helm-package
                       helm-projectile
                       helm-projectile-all
                       helm-rails
                       helm-spotify
                       helm-themes
                       ht
                       inflections
                       js2-mode
                       javap-mode
                       jinja2-mode
                       let-alist
                       list-utils
                       log4e
                       logito
                       lua-mode
                       magit-filenotify
                       magit-find-file
                       magit-gh-pulls
                       magit-gitflow
                       magithub
                       marshal
                       mode-icons
                       multi
                       nav-flash
                       noflet
                       nyan-mode
                       oauth2
                       org-present
                       paredit
                       password-generator
                       pcache
                       peg
                       persistent-soft
                       persp-projectile
                       perspective
                       pkg-info
                       popup
                       popwin
                       pos-tip
                       powerline
                       projectile-speedbar
                       protobuf-mode
                       python-django
                       pyvenv
                       queue
                       rainbow-delimiters
                       request
                       s
                       scheme-complete
                       scratch-palette
                       seq
                       session
                       slamhound
                       smartrep
                       spaceline
                       spinner
                       sr-speedbar
                       string-utils
                       tabbar
                       tabbar-ruler
                       tabulated-list
                       textmate
                       thrift
                       tide
                       twittering-mode
                       ucs-utils
                       yasnippet-bundle
                       yaml-mode

                       ;; Themes
                       clues-theme
                       color-theme
                       cyberpunk-theme
                       noctilux-theme
                       zenburn-theme
                       underwater-theme
                       subatomic-theme
                       firebelly-theme
                       color-theme-solarized
                       bubbleberry-theme
                       ample-zen-theme
                       ample-theme
                       afternoon-theme)
  "Selected packages")

;; Shamelessly taken from Aaron Bedra:
;; http://aaronbedra.com/emacs.d/
(defun abedra/packages-installed-p ()
  (loop for pkg in jmi/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (abedra/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg jmi/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;; packages.el ends here

