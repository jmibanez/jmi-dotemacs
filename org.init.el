;;; org.init.el -- Configuration for org-mode tasks and notes

;;; Commentary:

;;; I'm trying out org-mode after a hiatus of over a decade; I'll be
;;; seeing if I can use it to maintain a digital copy of notes and
;;; quick links (which I can later transform to wiki pages)

;;; Code:

(use-package org
  :config

  (defun jmi/org-clock-in-switch-to-in-progress (task-state)
    (if (string= task-state "TODO")
        "INPROGRESS"
      task-state))

  (defun jmi/org-clock-out-switch-to-todo (task-state)
    (if (string= task-state "INPROGRESS")
        "TODO"
      task-state))

  (setq jmi/org-task-dir "~/Documents/org")

  (setq org-startup-indented                   t
        ;; Let's not hide the leading stars, it gets annoying quickly
        org-indent-mode-turns-on-hiding-stars  nil

        ;; Enforce that parent tasks can only be marked DONE if all
        ;; child tasks are also DONE
        org-enforce-todo-dependencies          t
        org-enforce-todo-checkbox-dependencies t

        ;; Default TODO states
        org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "BLOCKED(b@/!)" "|" "DONE(d!)" "CANCELLED(c)"))

        ;; Automatically switch to INPROGRESS when clocking in
        org-clock-in-switch-to-state #'jmi/org-clock-in-switch-to-in-progress
        org-clock-out-switch-to-state #'jmi/org-clock-out-switch-to-todo

        ;; Agenda config -------
        ;; All my org files are in ~/Documents/org, so should be what
        ;; agenda looks at
        org-agenda-files                       (list jmi/org-task-dir)

        ;; Include diary into agenda, so I get anniversaries and appointments
        org-agenda-include-diary               t

        ;; org-capture templates
        org-capture-templates
        '(("t" "General task" entry (file+headline "~/Documents/org/inbox.org" "Tasks")
           "* TODO %?\n%i\n%a")
          ("T" "Ticket/Issue task" entry (file+headline "~/Documents/org/inbox.org" "Tasks")
           "* TODO [%^{SIM ID?}] %?\n[[issue:%\\1][%\\1]]\n%i\n%a")
          ("n" "Note" entry (file+headline "~/Documents/org/inbox.org" "Notes")
           "* %?\n%i\n%a")
          ("b" "WikiBlog" entry (file+datetree "~/Documents/org/blog.org")
           "* %?"
           :empty-lines 1))

        org-refile-targets '((org-agenda-files . (:level . 1))))

  ;; Archiving
  (setq org-archive-location "~/Documents/org-archive/%s::")

  ;; Helper fns for opening various org task files
  (defun jmi/open-org-inbox ()
    (interactive)
    (find-file (concat jmi/org-task-dir "/inbox.org")))

  (defun jmi/open-org-notes ()
    (interactive)
    (find-file (concat jmi/org-task-dir "/notes.org")))

  (defun jmi/tick-gnus-message-if-linked ()
    (if (string= "gnus" (plist-get org-store-link-plist :type))
        (let* ((message-id   (plist-get org-store-link-plist :message-id))
               (group        (plist-get org-store-link-plist :group))
               (group-buffer (concat "*Summary " group "*")))
          (message message-id)
          (with-current-buffer group-buffer
            (gnus-summary-goto-article message-id)
            (gnus-summary-tick-article)))
      (message "Not a gnus link")))

  :hook
  ;; I use windmove (with custom keybindings), so in general I don't
  ;; need this, but it's a good idea nonetheless to avoid conflict
  ;; with windmove's regular keybindings
  ;; ((org-shiftup-final    .  windmove-up)
  ;;  (org-shiftleft-final  .  windmove-left)
  ;;  (org-shiftdown-final  .  windmove-down)
  ;;  (org-shiftright-final .  windmove-right))

  ((org-capture-before-finalize . jmi/tick-gnus-message-if-linked))



  ;; Global key bindings
  :bind ((:map jmi/my-jump-keys-map
               ("l"      .  org-store-link)
               ("a"      .  org-agenda)
               ("t i"    .  jmi/open-org-inbox)
               ("t n"    .  jmi/open-org-notes)))

  :demand t)

;; org-roam as my KB on top of org in general
(use-package org-roam
  :commands (org-roam-dailies-capture-yesterday org-roam-dailies-capture-tomorrow)

  :config
  (require 'org-roam-dailies)
  (setq org-roam-directory "~/Documents/org-roam")
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %<%H:%M>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))))

  ;; Sync DB
  (org-roam-db-autosync-mode)

  :bind ((:map jmi/my-jump-keys-map
               ("r r"   . org-roam-buffer-toggle)
               ("<f8>"  . org-roam-capture)
               ("r f"   . org-roam-node-find)
               ("r i"   . org-roam-node-insert)
               ("r d"   . org-roam-dailies-map)
               ("d"     . org-roam-dailies-capture-today))
         (:map org-roam-dailies-map
               ("Y"   . org-roam-dailies-capture-yesterday)
               ("T"   . org-roam-dailies-capture-tomorrow)))


  :after (org))

;; Presentations
(use-package org-present
  :config
  (defun jmi/org-present-setup ()
    (visual-fill-column-mode 1)
    (visual-line-mode 1)
    (org-present-big)
    (org-display-inline-images)
    (internal-show-cursor (selected-window) nil))

  (defun jmi/org-present-teardown ()
    (visual-fill-column-mode 0)
    (visual-line-mode 0)
    (org-present-small)
    (org-remove-inline-images)
    (internal-show-cursor (selected-window) t))

  (defun jmi/org-present-prepare-slide (buffer-name heading)
    ;; Show only top-level headlines
    (org-overview)
    ;; Unfold the current entry
    (org-show-entry)
    ;; Show only direct subheadings of the slide, but don't expand them
    (org-show-children))

  ;; Because Reasons (use-package), we need to explicitly hook it
  ;; here, instead of using the :hook section
  (add-hook 'org-present-after-navigate-functions 'jmi/org-present-prepare-slide)

  :hook
  ((org-present-mode                     . jmi/org-present-setup)
   (org-present-quit                     . jmi/org-present-teardown))

  :after (org))

;;; org.init.el ends here
