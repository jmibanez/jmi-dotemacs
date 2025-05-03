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
  :config
  (setq eshell-info-banner-progress-bar-char "■")
  (setq eshell-info-banner-double-width-line-char "═")

  ;; Exclude macOS system volumes (Preboot, VM, etc.) except Data
  ;; which has the user home dir. Also ignore simulator images
  (when (eq system-type 'darwin)
    (setq eshell-info-banner-exclude-partitions
          '("Preboot"
            "VM"
            "Update"
            "watchOS"
            "iOS"
            "CoreSimulator"
            "private")))

  :ensure-system-package duf

  :hook (eshell-banner-load . eshell-info-banner-update-banner)
  :after eshell

  :vc (:url "https://github.com/jmibanez/eshell-info-banner.el"
       :branch "main"
       :rev :newest)

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

(use-package eshell-up)

;;; term.init.el ends here
