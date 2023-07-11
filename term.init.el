;;; term.init.el -- Config for shell/terminal support

;;; Commentary:

;;; Customizations for running my terminal sessions in Emacs,
;;; including some eshell customizations.  I've not yet decided whether
;;; to use eshell or vterm, but I'm leaning towards vterm at the
;;; moment.

;;; Code:


(require 'cl)

(defun jmi/list-directories-only (base-path)
  (if (not (file-directory-p base-path))
      '()

    (remove-if (lambda (f-or-d)
                 (or (string-equal "." f-or-d)
                     (string-equal ".." f-or-d)
                     (not (file-directory-p (concat base-path f-or-d)))))
               (directory-files base-path))))

(defun jmi/all-workspaces ()
  (let ((all-ws (jmi/list-directories-only "~/projects/")))
    (mapcan (lambda (ws)
              (loop for prj-in-ws in (jmi/list-directories-only (concat "~/projects/" ws))
                    collect (cons prj-in-ws ws)))
            all-ws)))

(defun jmi/search-workspaces (prj-name)
  (remove-if (lambda (prj-ws-tup)
               (not (string-equal (car prj-ws-tup)
                                  prj-name)))

             (jmi/all-workspaces)))

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'robbyrussell)

  :demand)


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

  (defun jmi/vterm-in-ssa ()
    (interactive)
    (jmi/vterm-in-dir "~/projects/SSA"))

  (defun jmi/vterm-in-home ()
    (interactive)
    (jmi/vterm-in-dir "~/"))

  (defun jmi/vterm-in-project-dir-maybe ()
    (interactive)
    (message "Current directory %s" default-directory)
    (if (string-match "projects/\\([A-Za-z0-9-]+\\)/\\([A-Za-z0-9-]+\\)/" default-directory)
        (let ((current-ws (match-string 1 default-directory))
              (current-prj (match-string 2 default-directory)))
          (progn
            (message "Project name %s" current-prj)
            (jmi/vterm-in-dir (format "~/projects/%s/%s" current-ws current-prj))))

      (progn
        (message "Opening term in %s" default-directory)
        (jmi/vterm-in-dir default-directory))))

  (setq vterm-buffer-name-string "*vterm* %s")
  (setq vterm-max-scrollback 100000)

  (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
  (define-key vterm-mode-map (kbd "S-<prior>") #'scroll-down-command)
  (define-key vterm-mode-map (kbd "S-<next>") #'scroll-up-command)
  (define-key vterm-mode-map (kbd "<f8>") nil)

  :bind
  (("C-s-t"   .  jmi/vterm-in-project-dir-maybe)
   (:map jmi/my-jump-keys-map
         ("t 0"   . jmi/vterm-in-home)
         ("t 1"   . jmi/vterm-in-ssa)))

  :hook (vterm-mode  . jmi/vterm-customizations)

  :demand)

;;; term.init.el ends here
