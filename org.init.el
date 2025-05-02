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
        ;; See org-roam config

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

  ;; Custom link: projects
  (declare-function projectile-switch-project-by-name "projectile")
  (defun jmi/org-project-open (path _)
    (if (not (featurep 'projectile))
        (require 'projectile))
    (projectile-switch-project-by-name path))

  (defun jmi/org-project-store-link ()
    (when (and (memq major-mode '(dired-mode))
               (project-current))
      (let* ((current-project (project-current))
             (current-project-path (cdr current-project))
             (description (format "Project %s" current-project-path))
             (link (format "project:%s" current-project-path)))
        (org-link-store-props
         :type "project"
         :link link
         :description description))))

  (defun jmi/org-project-export (link description format _)
    (let ((desc (or description link)))
      (pcase format
        (`html (format "<a href=\"file://%s\">%s</a>" path desc))
        (`latex (format "\\href{file://%s}{%s}" path desc))
        (`ascii (format "%s (%s)" desc path))
        (_ path))))

  (org-link-set-parameters "project"
                           :follow #'jmi/org-project-open
                           :export #'jmi/org-project-export
                           :store  #'jmi/org-project-store-link)


  (defun jmi/org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (defun jmi/org-update-statusbar-clock-in ()
    (do-applescript (format "tell application %S to clock in %S"
                            "org-clock-statusbar"
                            (substring-no-properties org-clock-current-task))))

  (defun jmi/org-update-statusbar-clock-out ()
    (do-applescript (format "tell application %S to clock out"
                            "org-clock-statusbar")))

  :hook
  ;; I use windmove (with custom keybindings), so in general I don't
  ;; need this, but it's a good idea nonetheless to avoid conflict
  ;; with windmove's regular keybindings
  ;; ((org-shiftup-final    .  windmove-up)
  ;;  (org-shiftleft-final  .  windmove-left)
  ;;  (org-shiftdown-final  .  windmove-down)
  ;;  (org-shiftright-final .  windmove-right))

  ((org-capture-before-finalize . jmi/tick-gnus-message-if-linked)
   (org-after-todo-statistics   . jmi/org-summary-todo)
   (org-clock-in-hook           . jmi/org-update-statusbar-clock-in)
   (org-clock-out-hook          . jmi/org-update-statusbar-clock-out))

  ;; Global key bindings
  :bind ((:map jmi/my-jump-keys-map
               ("l"      .  org-store-link)
               ("g c"    .  org-clock-goto)
               ("a"      .  org-agenda)))
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

  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))


  ;; functions borrowed from `vulpea' library
  ;; https://github.com/d12frosted/vulpea/blob/6a735c34f1f64e1f70da77989e9ce8da7864e5ff/vulpea-buffer.el
  (defun vulpea-buffer-tags-get ()
    "Return filetags value in current buffer."
    (vulpea-buffer-prop-get-list "filetags" "[ :]"))

  (defun vulpea-buffer-tags-set (&rest tags)
    "Set TAGS in current buffer.
If filetags value is already set, replace it."
    (if tags
        (vulpea-buffer-prop-set
         "filetags" (concat ":" (string-join tags ":") ":"))
      (vulpea-buffer-prop-remove "filetags")))

  (defun vulpea-buffer-tags-add (tag)
    "Add a TAG to filetags in current buffer."
    (let* ((tags (vulpea-buffer-tags-get))
           (tags (append tags (list tag))))
      (apply #'vulpea-buffer-tags-set tags)))

  (defun vulpea-buffer-tags-remove (tag)
    "Remove a TAG from filetags in current buffer."
    (let* ((tags (vulpea-buffer-tags-get))
           (tags (delete tag tags)))
      (apply #'vulpea-buffer-tags-set tags)))

  (defun vulpea-buffer-prop-set (name value)
    "Set a file property called NAME to VALUE in buffer file.
If the property is already set, replace its value."
    (setq name (downcase name))
    (org-with-point-at 1
      (let ((case-fold-search t))
        (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                               (point-max) t)
            (replace-match (concat "#+" name ": " value) 'fixedcase)
          (while (and (not (eobp))
                      (looking-at "^[#:]"))
            (if (save-excursion (end-of-line) (eobp))
                (progn
                  (end-of-line)
                  (insert "\n"))
              (forward-line)
              (beginning-of-line)))
          (insert "#+" name ": " value "\n")))))

  (defun vulpea-buffer-prop-set-list (name values &optional separators)
    "Set a file property called NAME to VALUES in current buffer.
VALUES are quoted and combined into single string using
`combine-and-quote-strings'.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t.
If the property is already set, replace its value."
    (vulpea-buffer-prop-set
     name (combine-and-quote-strings values separators)))

  (defun vulpea-buffer-prop-get (name)
    "Get a buffer property called NAME as a string."
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                               (point-max) t)
        (buffer-substring-no-properties
         (match-beginning 1)
         (match-end 1)))))

  (defun vulpea-buffer-prop-get-list (name &optional separators)
    "Get a buffer property NAME as a list using SEPARATORS.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
    (let ((value (vulpea-buffer-prop-get name)))
      (when (and value (not (string-empty-p value)))
        (split-string-and-unquote value separators))))

  (defun vulpea-buffer-prop-remove (name)
    "Remove a buffer property called NAME."
    (org-with-point-at 1
      (when (re-search-forward (concat "\\(^#\\+" name ":.*\n?\\)")
                               (point-max) t)
      (replace-match ""))))

  ;; Cribbed from https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
  (defun jmi/org-roam-note-is-project-p ()
    "Return non-nil if current buffer has any TODO entries.

TODO entries marked as done are ignored, meaning this function returns
nil if the current buffer contains only completed tasks."

    (org-element-map
        (org-element-parse-buffer 'headline)
        'headline
      (lambda (h)
        (eq (org-element-property :todo-type h)
            'todo))
      nil 'first-match))

  (defun jmi/org-roam-buffer-p ()
    "Return non-nil if the currently visited buffer is an org-roam note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun jmi/org-project-update-tag ()
    "Update PROJECT tag in the current buffer, if it is an org-roam buffer."

    (when (and (not (active-minibuffer-window))
               (jmi/org-roam-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (jmi/org-roam-note-is-project-p)
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))

          (setq tags (seq-uniq tags))

          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))


  (defun jmi/org-project-files ()
    "Return a list of org-roam notes containing 'project' tag."
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"project\"%"))]))))

  (defun jmi/update-agenda-files (&rest _)
    "Keep `org-agenda-files' up to date."
    (setq org-agenda-files (jmi/org-project-files)))

  (advice-add 'org-agenda :before #'jmi/update-agenda-files)
  (advice-add 'org-todo-list :before #'jmi/update-agenda-files)

  (setq org-agenda-files (jmi/org-project-files))

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


  :hook
  ((find-file     . jmi/org-project-update-tag)
   (before-save   . jmi/org-project-update-tag))

  :after (org))

(use-package ob-eshell
  :ensure nil
  :after (org))

(use-package ob-shell
  :ensure nil
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

;; Not org-related, but useful nonetheless. I probably should rename this module...
(use-package hyperbole
  :config
  (hyperbole-mode 1))

;;; org.init.el ends here
