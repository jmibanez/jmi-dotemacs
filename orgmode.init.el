;; orgmode

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/doc/personal/tasks/tasks.org"
                             "~/doc/personal/tasks/personal.org"
                             "~/doc/personal/tasks/links.org"
                             "~/doc/personal/tasks/onbsl.org"))

(setq org-directory "~/doc/personal/tasks/")
(setq org-default-notes-file "~/doc/personal/tasks/personal.org")

(setq remember-handler-functions '(org-remember-handler))
(setq remember-annotation-functions '(org-remember-annotation))

(add-hook 'remember-mode-hook 'org-remember-apply-template)

(setq org-remember-templates
      '((?n "* %?\n  %i\n  %a"
            "~/doc/personal/tasks/links.org")
        (?t "* TODO %?\n  %i\n  %a"
            "~/doc/personal/tasks/tasks.org"
            "Miscellany")
        (?j "* %T %?\n\n  %i\n  %a"
            "~/doc/personal/tasks/personal.org")))

;; TODO states
;;(setq org-todo-keywords
;;      '((sequence "TODO" "FEEDBACK" "|" "DONE" "DELEGATED" "CANCELLED")))



;;(setq org-todo-keywords
;;      '((sequence "TODO" "|" "DONE")
;;        (sequence "REPORT" "BUG" "KNOWNCAUSE" "|" "FIXED")
;;        (sequence "|" "CANCELED")))


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

;; Go map outline-magic
;; (add-hook 'outline-mode-hook
;;           (lambda ()
;;             (require 'outline-magic)
;;             (outline-cycle)))

(add-hook 'outline-minor-mode-hook
          (lambda ()
            (require 'outline-magic)
            (define-key outline-minor-mode-map [(f10)] 'outline-cycle)))

