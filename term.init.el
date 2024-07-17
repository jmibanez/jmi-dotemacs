;;; term.init.el -- Config for shell/terminal support

;;; Commentary:

;;; Customizations for running my terminal sessions in Emacs,
;;; including some eshell customizations.  I've not yet decided whether
;;; to use eshell or vterm, but I'm leaning towards vterm at the
;;; moment.

;;; Code:


(require 'cl)
(require 'seq)

(defun jmi/list-directories-only (base-path)
  (if (not (file-directory-p base-path))
      '()

    (remove-if (lambda (f-or-d)
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

  (setq eshell-aliases-file (concat jmi/my-emacs-init-path "eshell/aliases"))
  (setq eshell-rc-script (concat jmi/my-emacs-init-path "eshell/rc"))

  (esh-command prj (prj-name ws-name)
               (:command
                (if ws-name
                    (if (string-equal ws-name "personal")
                        (eshell/cd (concat "~/projects/personal/" prj-name))
                      (eshell/cd (concat "~/projects/" ws-name "/src/" prj-name)))

                  (progn
                    (let ((ws-prj-matches
                           (jmi/search-workspaces prj-name))
                          (current-ws (jmi/current-workspace)))

                      (if (length> ws-prj-matches 1)

                          ;; Check local workspace
                          (let ((in-ws-pkg-path (concat current-ws "/src/" prj-name)))
                            (if (file-directory-p in-ws-pkg-path)
                                (eshell/cd in-ws-pkg-path)
                              (message "Duplicate projects for %s; specify workspace: (matches: %s)"
                                       prj-name
                                       (mapcar #'cdr ws-prj-matches))))

                        (let* ((match-prj-ws-tup (first ws-prj-matches))
                               (match-ws-name (cdr match-prj-ws-tup))
                               (match-prj-name (car match-prj-ws-tup)))
                          (eshell/cd (concat "~/projects/" match-ws-name "/src/" match-prj-name))))))))
               (:completion
                ;; completion
                (let ((project-ws-map (jmi/all-projects-across-workspaces))
                      (current-ws (jmi/current-workspace)))
                  (pcomplete-here*
                   (if current-ws
                       (mapcar (lambda (p)
                                 (-> p
                                     cdr
                                     file-name-nondirectory))
                               (amz-workspace-currently-mapped-package-sources current-ws))
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


  (defun pcomplete/ebb ()
    (pcomplete-here* '("build" "release" "test")))

  :bind
  (("C-s-t" .   eshell))

  :ensure nil)

(use-package eshell-info-banner
  :config
  ;; Add additional macOS versions
  (add-to-list 'eshell-info-banner--macos-versions
               '("^13\\."        . "macOS Ventura"))
  (add-to-list 'eshell-info-banner--macos-versions
               '("^14\\."        . "macOS Sonoma"))
  (add-to-list 'eshell-info-banner--macos-versions
               '("^15\\."        . "macOS Sequoia"))

  ;; Annoyingly, we need to re-eval/redefine this function, since the
  ;; implementation uses a backquote to effectively cache the version
  ;; at eval time.
  (defun eshell-info-banner--get-os-information-darwin ()
    "See `eshell-info-banner--get-os-information'."
    `(,(eshell-info-banner--get-macos-name
        (s-trim
         (eshell-info-banner--shell-command-to-string "sw_vers -productVersion")))
      .
      ,(s-trim (eshell-info-banner--shell-command-to-string "uname -rs"))))

  (setq eshell-info-banner-progress-bar-char "=")

  ;; Exclude macOS system volumes (Preboot, VM, etc.) except Data
  ;; which has the user home dir
  (when (eq system-type 'darwin)
    (setq eshell-info-banner-exclude-partitions
          '("Preboot"
            "VM"
            "Update")))

  :ensure-system-package duf

  :hook (eshell-banner-load . eshell-info-banner-update-banner)
  :after eshell

  :demand t)


(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'robbyrussell)

  :demand)

(use-package eshell-prompt-extras
  :config

  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-multiline-with-status
        epe-path-style 'full)

  (epe-theme-dakrone)

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
