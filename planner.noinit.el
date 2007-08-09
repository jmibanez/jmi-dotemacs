;; Customizations for planner-el

;; Add requirement for loading planner

(require 'planner)
(require 'planner-notes-index)
(require 'planner-rss)
(require 'planner-calendar)
(require 'planner-diary)
(require 'planner-cyclic)
(require 'planner-deadline)
(require 'planner-gnus)
(require 'planner-w3m)
(require 'planner-ical)

(require 'remember-planner)

;; (planner-gnus-insinuate)

;; Load remember hooks/advice
(load-file "~/.emacs.remember.el")


;; ==== Following is from my config for Sacha's planner-el tree;
;; ==== converting to muse-based planner-el

;; Diary integration into planner
;; == commented_out == (add-hook 'diary-display-hook 'fancy-diary-display)
;; == commented_out == (setq planner-diary-use-diary t)
;; == commented_out == (planner-diary-insinuate)
;; == commented_out == (setq mark-diary-entries-in-calendar t)
;; == commented_out == 
;; == commented_out == (setq planner-carry-tasks-forward t)
;; == commented_out == (setq planner-default-task-priority "B")
;; == commented_out == (setq planner-use-task-numbers t)
;; == commented_out == (setq planner-renumber-tasks-automatically t)
;; == commented_out == (setq planner-renumber-notes-automatically t)
;; == commented_out == (setq planner-reverse-chronological-notes nil)
;; == commented_out == (setq planner-add-task-at-end-flag t)
;; == commented_out == (setq planner-day-page-template "* Tasks
;; == commented_out == 
;; == commented_out == 
;; == commented_out == * Schedule
;; == commented_out == 
;; == commented_out == 
;; == commented_out == * Diary
;; == commented_out == 
;; == commented_out == 
;; == commented_out == * Notes
;; == commented_out == 
;; == commented_out == 
;; == commented_out == ")
;; == commented_out == (setq planner-directory "~/doc/personal/plans")
;; == commented_out == 
;; == commented_out == ;; Planner calendar
;; == commented_out == (add-hook
;; == commented_out ==  'planner-mode-hook
;; == commented_out ==       (lambda ()
;; == commented_out ==         "Add the relevant hooks for `planner-calendar' to work."
;; == commented_out ==         (add-hook 'emacs-wiki-before-markup-hook
;; == commented_out ==                   'planner-calendar-insert-calendar-maybe nil t)
;; == commented_out ==         (add-hook 'emacs-wiki-after-file-publish
;; == commented_out ==                   'planner-calendar-create-today-link nil t)
;; == commented_out ==         (add-hook 'emacs-wiki-after-markup-hook
;; == commented_out ==                   'planner-calendar-move-calendar-to-top-of-page-maybe nil t)))
;; == commented_out == 
;; == commented_out == (setq planner-calendar-prev-month-button "<<")
;; == commented_out == (setq planner-calendar-next-month-button ">>")
;; == commented_out == 
;; == commented_out == (setq remember-handler-functions '(remember-planner-append))
;; == commented_out == (setq remember-annotation-functions planner-annotation-functions)
;; == commented_out == 
;; == commented_out == ;; Planner-cyclic
;; == commented_out == (setq planner-cyclic-diary-file "~/diary.cyclic")
;; == commented_out == (setq planner-cyclic-diary-nag nil)


;; Use new Muse stuff

(setq planner-project "JmIbanezPlans")
(setq muse-project-alist
      '(("JmIbanezPlans"
         ("~/doc/personal/plans"
          :default "TaskPool"
          :major-mode planner-mode
          :visit-link planner-visit-link)

         (:base "planner-xhtml"
                :path "~/public_html/plans"))))

;; Insinuate planner into diary
;; (add-hook 'diary-display-hook 'fancy-diary-display)
;; (setq planner-diary-use-diary t)
;; (planner-diary-insinuate)
;; (setq mark-diary-entries-in-calendar t)

;; General planner settings
(setq planner-carry-tasks-forward 3)
(setq planner-default-task-priority "B")
(setq planner-use-task-numbers t)
(setq planner-renumber-tasks-automatically t)
(setq planner-renumber-notes-automatically t)
(setq planner-reverse-chronological-notes nil)
(setq planner-add-task-at-end-flag t)

;; remember-el integration
;; (setq remember-handler-functions '(remember-planner-append))
;; (setq remember-annotation-functions planner-annotation-functions)


;; Customize annotations
(setq planner-annotation-strip-directory t)
(setq planner-annotation-use-relative-file t)

;; From info: Export ical

;; Appointments
(planner-appt-use-tasks-and-schedule)

;; display today
;; (plan 3)
