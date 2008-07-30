;; orgmode

;; We're using upstream instead of the Emacs distribution; as such,
;; use the one in our loadpath
(add-to-list 'load-path "~/elisp/org-mode")
(load-file "~/elisp/org-mode/org-install.elc")

(require 'org)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


(setq jmi/default-task-directory
      "~/doc/personal/tasks/")

(defun jmi/traverse-and-list-task-directory (task-dir)
  (directory-files task-dir t "\\.org$"))

(setq org-agenda-files (jmi/traverse-and-list-task-directory jmi/default-task-directory))

;;(setq org-agenda-files '("~/doc/personal/tasks/tasks.org"
;;                         "~/doc/personal/tasks/personal.org"
;;                         "~/doc/personal/tasks/work.org"))

(setq org-directory "~/doc/personal/tasks/")
(setq org-default-notes-file "~/doc/personal/tasks/notes.org")

(setq remember-handler-functions '(org-remember-handler))
(setq remember-annotation-functions '(org-remember-annotation))

(add-hook 'remember-mode-hook 'org-remember-apply-template)

(setq org-remember-templates
      '(("Tasks" ?t "* TODO %?\n  %i\n  %a"
         "~/doc/personal/tasks/inbox.org"
         "Tasks")
        ("Work Ticket" ?i "* TICKET %?\n  %i\n  %a"
         "~/doc/personal/tasks/inbox.org"
         "Tickets")
        ("Journal" ?j "* %T %?\n\n  %i\n  %a"
         "~/doc/personal/tasks/inbox.org"
         "Journal")
        ("Note" ?n "* %?\n  %i\n  %a"
         "~/doc/personal/tasks/inbox.org"
         "Notes")))

;; TODO states
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE" "DELEGATED" "CANCELLED")
        (sequence "TICKET" "ACCEPTED" "|" "CLOSED")))

;;(setq org-todo-keywords
;;      '((sequence "TODO" "|" "DONE")
;;        (sequence "REPORT" "BUG" "KNOWNCAUSE" "|" "FIXED")
;;        (sequence "|" "CANCELED")))


;; Stuck projects
(setq org-stuck-projects
      '("+LEVEL=2/-DONE"
        ("TODO" "WAITING")
        nil
        ""))


;; Insinuate diary
(setq org-agenda-include-diary t)


;; Turn on org-mode table editing
(add-hook 'mail-mode-hook 'turn-on-orgtbl)

(setq org-ellipsis " ==>")

;; Hide leading stars
(setq org-hide-leading-stars t)

;; Archive
(setq org-archive-location
      "~/doc/personal/tasks/archive/%s::")

;; Abbrevs (Trac, etc.)
(setq org-link-abbrev-alist
      '(("trac" . "https://issues.orangeandbronze.com/")))



;; Fix Gnus to display in the same frame, possibly
(if (assq 'gnus org-link-frame-setup)
    (setcdr (assq 'gnus org-link-frame-setup)
            #'gnus)
  (append org-link-frame-setup '((gnus . #'gnus))))

;; Go map outline-magic
;; (add-hook 'outline-mode-hook
;;           (lambda ()
;;             (require 'outline-magic)
;;             (outline-cycle)))

(add-hook 'outline-minor-mode-hook
          (lambda ()
            (require 'outline-magic)
            (define-key outline-minor-mode-map [(f10)] 'outline-cycle)))

;; Email link: No subject lines
(setq org-email-link-description-format "Email %c")

;; Newsticker customization -- hook scripts

(defun jmi/newsticker-gen-orgmode-link ()
  (let* ((buffer (current-buffer)))
    ;; Get the mode of the current buffer
    (if (eq major-mode 'newsticker-mode)
        (progn
          (message "Got link")
          (identity "rss::")))))


(add-hook 'org-create-file-search-functions
          #'jmi/newsticker-gen-orgmode-link)


;; Rescan .org files
(defun jmi/org-rescan-agenda-files ()
  "Rescan and rebuild org-agenda-files list"
  (interactive)
  (jmi/traverse-and-list-task-directory jmi/default-task-directory))

;; Automatically add .org files in the jmi/default-task-directory

(setq jmi/new-org-buffers-list nil)

(defun jmi/notice-new-org-file ()
  (let* ((buffer (current-buffer))
         (bfile-name (buffer-file-name buffer)))
    (if (and (string-match "\\.org$" bfile-name)
             (equal (file-name-directory bfile-name)
                    (expand-file-name jmi/default-task-directory)))
        (progn
          (setq jmi/new-org-mode-file-p t)
          (message "Noticed %s" bfile-name)
          (add-to-list 'jmi/new-org-buffers-list buffer)
          nil))))

(defun jmi/add-new-org-file-to-list ()
  (let* ((buffer (current-buffer))
         (bfile-name (buffer-file-name buffer)))
    (if (memq buffer jmi/new-org-buffers-list)
        (progn
          (add-to-list 'org-agenda-files bfile-name)
          (setq jmi/new-org-buffers-list (delete buffer jmi/new-org-buffers-list))))))

;; Hook to new file visits
(add-hook 'find-file-not-found-functions 'jmi/notice-new-org-file)
(add-hook 'after-save-hook 'jmi/add-new-org-file-to-list)
