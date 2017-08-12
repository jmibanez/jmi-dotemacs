;; Entry point for all my start files

;; Start server
(server-start)
;; (gnuserv-start)

;; As early as possible, disable menu, scroll, and tool bar
(tool-bar-mode -1)

;; If not on macOS, turn off the menu bar
(if (not (eq system-type 'darwin))
    (menu-bar-mode -1))
(scroll-bar-mode -1)

;; Blink the cursor
(blink-cursor-mode 1)

;; Basic init funs
(defmacro jmi/dotemacs-do-module (filename)
  (load-file (concat jmi/my-emacs-init-path filename)))


(defun jmi/list-init-files (directory)
  (let (init-files-list
        (current-dir-list (directory-files-and-attributes directory t)))
    (dolist (dir-item current-dir-list init-files-list)
      (if (equal ".init.el" (substring (car dir-item) -8))
          (setq init-files-list
                (cons (car dir-item)
                      init-files-list))))))

;; Load paths
(jmi/dotemacs-do-module "loadpaths.el")

;; Packages/Packaging
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(defvar jmi/packages '(2048-game
                       ac-cider-compliment
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
                       helm-projectile-all
                       helm-rails
                       helm-spotify
                       helm-themes
                       ht
                       inflections
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
                       spinner
                       sr-speedbar
                       string-utils
                       tabbar
                       tabbar-ruler
                       tabulated-list
                       thrift
                       twittering-mode
                       ucs-utils
                       yasnippet-bundle
 
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

(set-frame-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)


;; Load all init modules
(mapc 'load-file
      (sort (jmi/list-init-files jmi/my-emacs-init-path)
            'string-lessp))

;; Load keybindings
(jmi/dotemacs-do-module "keybindings.el")

