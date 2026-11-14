;;; term.init.el -- Config for shell/terminal support  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Customizations for running my terminal sessions in Emacs,
;;; including some eshell customizations. After more than a year of
;;; using eshell, I'm sold and only keep ghostel around for the
;;; occasional tool that needs a full terminal emulator.

;;; Code:

(use-package xterm-color
  :config
  (setopt xterm-color-use-bold-for-bright t))

(use-package eshell
  :init
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

  :config
  (require 'cl-lib)
  (require 'seq)

  (defun jmi/browse-esh-history ()
    (interactive)
    (require 'em-hist)
    (consult-history eshell-history-ring))

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
                (cl-loop for prj-in-ws in (jmi/list-directories-only (concat "~/projects/" ws "/"))
                      collect (cons prj-in-ws ws)))
              all-ws)))

  (defun jmi/all-workspaces ()
    (jmi/list-directories-only "~/projects/"))

  (defun jmi/current-workspace ()
    (concat "~/projects/"
            (cl-first
             (seq-filter (lambda (ws-name)
                           (string-prefix-p (file-truename (concat "~/projects/" ws-name))
                                            (file-truename default-directory)))
                         (jmi/all-workspaces)))))

  (defun jmi/projects-in-workspace (ws-path)
    (seq-filter (lambda (project-path)
                  (string-prefix-p (file-truename ws-path)
                                   (file-truename project-path)))
                (projectile-known-projects)))

  (defun jmi/search-workspaces (prj-name)
    (seq-filter (lambda (prj-ws-tup)
                  (string-equal (car prj-ws-tup)
                                prj-name))
                (jmi/all-projects-across-workspaces)))


  (require 'em-smart)
  (require 'pcomplete)
  (setopt eshell-where-to-jump            'begin
          eshell-review-quick-commands    nil
          eshell-smart-space-goes-to-end  t

          ;; A history ring size of 128 is too small; use something like 1k
          ;; entries (which should be fine). Consider bumping this up to
          ;; something like 4k or 10k...
          eshell-history-size             1000

          eshell-aliases-file             (concat jmi/my-emacs-init-path "eshell/aliases")
          eshell-rc-script                (concat jmi/my-emacs-init-path "eshell/rc"))

  (add-to-list 'eshell-modules-list 'eshell-tramp)

  (esh-command prj (prj-name ws-name)
               (:command
                (let ((candidates (cl-remove-if-not (lambda (p)
                                                      (string-suffix-p (concat "/" prj-name "/") p))
                                                    (projectile-known-projects))))
                  (message (format "%s" (length candidates)))
                  (cond ((length= candidates 1)
                         (eshell/cd (car candidates)))

                        ((length> candidates 1)
                         (eshell/cd (completing-read "Multiple candidate projects: "
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
                                     cl-second))
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
  (("C-s-t" .   eshell)
   (:map eshell-mode-map
         ("s-r" . jmi/browse-esh-history)))

  :hook ((kill-emacs . eshell-save-some-history))
  :ensure nil)

(use-package eshell-info-banner
  :config
  (setopt eshell-info-banner-progress-bar-char      "■"
          eshell-info-banner-double-width-line-char "═"
          eshell-info-banner-right-padding-char     "·"
          eshell-info-banner-column-separator-char  " ")

  ;; Exclude macOS system volumes (Preboot, VM, etc.) except Data
  ;; which has the user home dir. Also ignore simulator images
  (when (eq system-type 'darwin)
    (setopt eshell-info-banner-exclude-partitions
            '("Preboot"
              "VM"
              "Update"
              "watchOS"
              "iOS"
              "CoreSimulator"
              "SimRuntimeBundle"
              "private")))

  :ensure-system-package duf
  :ensure nil

  :hook (eshell-banner-load . eshell-info-banner-update-banner)
  :after eshell
  :vc-or-local (:url "https://github.com/jmibanez/eshell-info-banner.el"
                :local-path "~/projects/personal/eshell-info-banner.el/"
                :branch "main"))

(use-package eshell-prompt-extras
  :config
  (setopt eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-multiline-with-status
          epe-path-style 'full)

  :after esh-opt)


(use-package ghostel
  :init
  ;; Keep the native module outside the package dir, so package
  ;; upgrades don't wipe it out from under us
  (setopt ghostel-module-directory    (locate-user-emacs-file "ghostel/")
          ghostel-module-auto-install 'compile)

  :config
  (defun jmi/ghostel-in-dir (dir &optional buf-name)
    (let ((default-directory dir)
          (ghostel-buffer-name (or buf-name (format "*ghostel %s*" dir))))
      (with-existing-directory
        (ghostel t))))

  (defun jmi/ghostel-in-home ()
    (interactive)
    (jmi/ghostel-in-dir "~/"))

  ;; ~5MB per 7k rows; roughly the 100k lines I had with vterm
  (setopt ghostel-max-scrollback (* 64 1024 1024))

  ;; The semi-char map sends these to the terminal, shadowing
  ;; ghostel-mode-map and my global <f8> prefix; let them fall through
  (setopt ghostel-keymap-exceptions
          (seq-union ghostel-keymap-exceptions
                     '("<f8>" "S-<prior>" "S-<next>")))

  (define-key ghostel-mode-map (kbd "S-<prior>") #'scroll-down-command)
  (define-key ghostel-mode-map (kbd "S-<next>") #'scroll-up-command)

  :bind
  ((:map jmi/my-jump-keys-map
         ("t 0"   . jmi/ghostel-in-home)))

  :vc (:url "https://github.com/dakra/ghostel"
            :lisp-dir "lisp"
            :rev
            :newest)
  :ensure-system-package (zig))

(use-package ghostel-eshell
  :config
  (ghostel-eshell-visual-command-mode 1)

  ;; Like eshell-vterm, a bare `v' opens a fresh terminal here
  (defun eshell/v (&rest args)
    (if args
        (apply #'eshell/ghostel args)
      (ghostel t)
      nil))

  :after eshell
  :ensure nil)

(use-package tramp
  :config
  (setq tramp-use-connection-share t)

  ;; Honor .dir-locals.el on remote hosts too, so per-project config
  ;; works the same on a remote checkout as on a local one
  (setopt enable-remote-dir-locals t)

  ;; Ignore auth-sources when doing sudo via Tramp
  (connection-local-set-profile-variables
   'tramp-sudo-without-auth-sources '((auth-sources . nil)))
  (connection-local-set-profiles
   '(:application tramp :protocol "sudo") 'tramp-sudo-without-auth-sources)

  :defer t
  :ensure nil)

(use-package eshell-toggle
  :config
  (setopt eshell-toggle-find-project-root-package 'projectile)
  (setopt eshell-toggle-default-directory "~/")
  (setopt eshell-toggle-run-command nil)

  :bind
  (("C-`"    . eshell-toggle)))

(use-package eshell-up)

;;; term.init.el ends here
