;;; term.init.el -- Config for shell/terminal support

;;; Commentary:

;;; Customizations for running my terminal sessions in Emacs,
;;; including some eshell customizations. After more than a year of
;;; using eshell, I'm sold and only keep vterm around for the
;;; occasional tool that needs a full terminal emulator.

;;; Code:


(require 'cl)
(require 'seq)

(defun jmi/list-directories-only (base-path)
  (if (not (file-directory-p base-path))
      '()

    (cl-remove-if (lambda (f-or-d)
                    (or (string-equal "." f-or-d)
                        (string-equal ".." f-or-d)
                        (not (file-directory-p (concat base-path f-or-d)))))
                  (directory-files base-path))))

(defun jmi/all-projects-across-workspaces ()
  (let ((all-ws (jmi/list-directories-only "~/projects/")))
    (mapcan (lambda (ws)
              (loop for prj-in-ws in (jmi/list-directories-only (concat "~/projects/" ws "/src/"))
                    collect (cons prj-in-ws ws)))
            all-ws)))

(defun jmi/all-workspaces ()
  (jmi/list-directories-only "~/projects/"))

(defun jmi/current-workspace ()
  (concat "~/projects/"
          (first
           (seq-filter (lambda (ws-name)
                         (string-prefix-p (file-truename (concat "~/projects/" ws-name))
                                          (file-truename default-directory)))
                       (jmi/all-workspaces)))))

(defun jmi/projects-in-workspace (ws-path)
  (seq-filter (lambda (project-path)
                (string-prefix-p (file-truename ws-path)
                                 (file-truename project-path)))
              projectile-known-projects))

(defun jmi/search-workspaces (prj-name)
  (seq-filter (lambda (prj-ws-tup)
                (string-equal (car prj-ws-tup)
                              prj-name))
              (jmi/all-projects-across-workspaces)))

(defmacro esh-command (alias arglist &rest command-def-alist)
  (let ((command-body (cdr (assoc :command command-def-alist)))
        (pcomplete-body (cdr (assoc :completion command-def-alist)))
        (real-fn-name (intern (concat "jmi/eshell-" (symbol-name alias))))
        (pcomplete-name (intern (concat "pcomplete/" (symbol-name alias)))))

    `(progn
       (defun ,real-fn-name ,arglist
         ,@command-body)
       ,(if pcomplete-body
            `(defun ,pcomplete-name ()
               ,@pcomplete-body))
       (defalias (quote
                  ,(intern (concat "eshell/" (symbol-name alias))))
         (quote ,real-fn-name)))))

(use-package eshell
  :config
  (require 'em-smart)
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)

  ;; A history ring size of 128 is too small; use something like 1k
  ;; entries (which should be fine). Consider bumping this up to
  ;; something like 4k or 10k...
  (setq eshell-history-size 1000)

  (setq eshell-aliases-file (concat jmi/my-emacs-init-path "eshell/aliases"))
  (setq eshell-rc-script (concat jmi/my-emacs-init-path "eshell/rc"))

  (esh-command prj (prj-name ws-name)
               (:command
                (let ((candidates (cl-remove-if-not (lambda (p)
                                                      (string-suffix-p (concat "/" prj-name "/") p))
                                                    projectile-known-projects)))
                  (message (format "%s" (length candidates)))
                  (cond ((length= candidates 1)
                         (eshell/cd (car candidates)))

                        ((length> candidates 1)
                         (eshell/cd (completing-read "Multiple candidate ptojects: "
                                                     candidates)))

                        ((and ws-name
                              (length> candidates 1))
                         (eshell/cd (cl-find-if (lambda (p) (string-match-p (regexp-quote ws-name) p))
                                                candidates)))

                        (t (error (format "No matching project %s in %s"
                                          prj-name ws-name))))))
               (:completion
                ;; completion
                (let ((project-ws-map (jmi/all-projects-across-workspaces))
                      (current-ws (jmi/current-workspace)))
                  (pcomplete-here*
                   (if current-ws
                       (mapcar (lambda (p)
                                 (-> p
                                     file-name-split
                                     reverse
                                     second))
                               (jmi/projects-in-workspace current-ws))
                     (mapcar 'car
                             project-ws-map)))
                  (pcomplete-here*
                   (mapcar 'cdr
                           (seq-filter (lambda (elt)
                                         (s-equals-p (car elt) (pcomplete-arg 1)))
                                       project-ws-map))))))

  (esh-command ws (&optional ws-name)
               (:command
                (eshell/cd (concat "~/projects/" ws-name)))
               (:completion
                (pcomplete-here* (jmi/all-workspaces))))

  (esh-command tailf (f)
               (:command
                (progn
                  (message "Viewing %s" f)
                  (view-file f)
                  (auto-revert-tail-mode))))

  (esh-command taillog (&optional logdir)
               (:command
                (let* ((dir (or logdir default-directory))
                       (last-log-file (car (mapcar #'car
                                                   (cl-sort (directory-files-and-attributes dir)
                                                            #'(lambda (x y) (not (time-less-p x y)))
                                                            :key #'(lambda (x) (nth 6 x)))))))

                  (jmi/eshell-tailf (concat (file-name-as-directory dir)
                                            last-log-file))))
               (:completion
                (pcomplete-here* default-directory)))


  :bind
  (("C-s-t" .   eshell))

  :ensure nil)

(use-package eshell-info-banner
  :init
  ;; Annoyingly, we need to re-eval/redefine these, the implementation
  ;; of eshell-info-banner--get-os-information-darwin uses a backquote
  ;; to effectively cache the version at eval time, and
  ;; eshell-info-banner--macos-versions doesn't get loaded until
  ;; --get-macos-name is compiled.
  (eval-when-compile
    (defconst jmi/eshell-info-banner--macos-versions
      '(("^10\\.0\\."  . "Mac OS X Cheetah")
        ("^10\\.1\\."  . "Mac OS X Puma")
        ("^10\\.2\\."  . "Mac OS X Jaguar")
        ("^10\\.3\\."  . "Mac OS X Panther")
        ("^10\\.4\\."  . "Mac OS X Tiger")
        ("^10\\.5\\."  . "Mac OS X Leopard")
        ("^10\\.6\\."  . "Mac OS X Snow Leopard")
        ("^10\\.7\\."  . "Mac OS X Lion")
        ("^10\\.8\\."  . "OS X Mountain Lion")
        ("^10\\.9\\."  . "OS X Mavericks")
        ("^10\\.10\\." . "OS X Yosemite")
        ("^10\\.11\\." . "OS X El Capitan")
        ("^10\\.12\\." . "macOS Sierra")
        ("^10\\.13\\." . "macOS High Sierra")
        ("^10\\.14\\." . "macOS Mojave")
        ("^10\\.15\\." . "macOS Catalina")
        ("^10\\.16\\." . "macOS Big Sur")
        ("^11\\."      . "macOS Big Sur")
        ("^12\\."      . "macOS Monterey")
        ("^13\\."      . "macOS Ventura")
        ("^14\\."      . "macOS Sonoma")
        ("^15\\."      . "macOS Sequoia"))
    "Versions of OSX and macOS and their name."))

  (defmacro jmi/eshell-info-banner--get-macos-name (version)
    "Get the name of the current macOS or OSX system based on its VERSION."
    `(cond
      ,@(mapcar (lambda (major)
                  `((string-match-p ,(car major)
                                    ,version)
                    ,(cdr major)))
                jmi/eshell-info-banner--macos-versions)
      (t "unknown version")))

  (defun jmi/eshell-info-banner--get-os-information-darwin ()
    "See `eshell-info-banner--get-os-information-darwin'."
    `(,(jmi/eshell-info-banner--get-macos-name
        (s-trim
         (eshell-info-banner--shell-command-to-string "sw_vers -productVersion")))
      .
      ,(s-trim (eshell-info-banner--shell-command-to-string "uname -rs"))))

  :config
  (advice-add 'eshell-info-banner--get-os-information-darwin :override
              #'jmi/eshell-info-banner--get-os-information-darwin)


  (setq eshell-info-banner-progress-bar-char "=")

  ;; Exclude macOS system volumes (Preboot, VM, etc.) except Data
  ;; which has the user home dir. Also ignore simulator images
  (when (eq system-type 'darwin)
    (setq eshell-info-banner-exclude-partitions
          '("Preboot"
            "VM"
            "Update"
            "watchOS"
            "iOS")))

  :ensure-system-package duf

  :hook (eshell-banner-load . eshell-info-banner-update-banner)
  :after eshell

  :demand t)


(use-package eshell-prompt-extras
  :config

  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-multiline-with-status
        epe-path-style 'full)

  :after esh-opt
  :demand t)


(use-package vterm
  :config

  (defun jmi/vterm-customizations ()
    (hi-lock-mode)
    (goto-address-mode)
    (highlight-regexp "Tests run: .*, Failures: [1-9].*" 'hi-yellow)
    (highlight-regexp "Tests run: .*, Failures: .*, Errors: [1-9].*" 'hi-red-b))

  (defun jmi/vterm-in-dir (dir &optional buf-name)
    (let ((default-directory dir))
      (with-existing-directory
        (vterm buf-name))))

  (defun jmi/vterm-in-home ()
    (interactive)
    (jmi/vterm-in-dir "~/"))

  (setq vterm-max-scrollback 100000)

  (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
  (define-key vterm-mode-map (kbd "S-<prior>") #'scroll-down-command)
  (define-key vterm-mode-map (kbd "S-<next>") #'scroll-up-command)
  (define-key vterm-mode-map (kbd "<f8>") nil)

  :bind
  ((:map jmi/my-jump-keys-map
         ("t 0"   . jmi/vterm-in-home)))

  :hook (vterm-mode  . jmi/vterm-customizations)

  :ensure-system-package cmake
  :demand)

(use-package eshell-vterm
  :config
  (eshell-vterm-mode)

  (defalias 'eshell/v 'eshell-exec-visual)

  :after eshell
  :requires vterm)

(use-package tramp
  :config
  (add-to-list 'eshell-modules-list 'eshell-tramp)

  (setq tramp-ssh-controlmaster-options
        (concat "-o ControlPath=~/ssh-ControlPath-%%r@%%h:%%p "
                "-o ControlMaster=yes -o ControlPersist=yes"))

  :after eshell
  :ensure nil)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)

  :after eshell)

(use-package eshell-toggle
  :config
  (setq eshell-toggle-find-project-root-package 'projectile)
  (setq eshell-toggle-default-directory "~/")
  (setq eshell-toggle-run-command nil)

  :bind
  (("C-`"    . eshell-toggle)))

;;; term.init.el ends here
