;;; keybindings.el -- Keybindings and keybinding suport

;;; Commentary:
;;; This just adds some custom keybindings etcetera, outside of those
;;; specific to certain modes

;;; Code:

(defun jmi/open-tasks (key)
  "Open org-mode file based on key"
  (interactive "cWhich task [i:Inbox w:Work p:Personal n:Notes o:Other...]")
  (let ((org-mode-task-dir "~/Documents/personal/tasks/")
        (is-org-mode (lambda (s) (equals (substring s -4) ".org"))))
    (case key
      (?i (find-file (concat org-mode-task-dir "inbox.org")))
      (?w (find-file (concat org-mode-task-dir "work.org")))
      (?p (find-file (concat org-mode-task-dir "personal.org")))
      (?n (find-file (concat org-mode-task-dir "notes.org")))
      (?o (find-file (read-file-name "Org file: " org-mode-task-dir nil t)))
      (otherwise (message "Task key not found.")))))

(defun jmi/open-tasks-notes (key)
  (interactive "cNote page [a:ABS-CBN b:BNSP i:ISAP-COCAF v:VOIP-Class]")
  (let ((org-mode-task-dir "~/Documents/personal/task-notes/work-projects/"))
    (case key
      (?a (find-file (concat org-mode-task-dir "abs-cbn.org")))
      (?b (find-file (concat org-mode-task-dir "bnsp.org")))
      (?i (find-file (concat org-mode-task-dir "isapcocaf.org")))
      (?v (find-file (concat org-mode-task-dir "voip.org")))
      (otherwise (message "Task key not found.")))))

;; User function: Toggle http_proxy in process-environment. Used so I
;; can go online etc. via w3m at ABS-CBN.

(defun jmi/toggle-http-proxy ()
  "Toggle the value/existence of http_proxy and the TSOCKS proxy
in process_environment."
  (interactive)
  (if (getenv "http_proxy")
      (setenv "http_proxy" nil)
    (setenv "http_proxy" "http://localhost:8888"))
  (jmi/do-toggle-tsocks)
  (message (let ((proxy (getenv "http_proxy")))
             (cond (proxy)
                   ("ENV: Unset proxy")))))

;; FIXME: Proper check on LD_PRELOAD
(defun jmi/do-toggle-tsocks ()
  "Toggle LD_PRELOAD for tsocks"
  (if (and t;; (getenv "LD_PRELOAD") ;; Stupid hack to get LD_PRELOAD synched with http_proxy
           (getenv "http_proxy"))
      (setenv "LD_PRELOAD" "/usr/lib/libtsocks.so")
    (setenv "LD_PRELOAD" nil)))


;; (global-set-key "\M-g" 'goto-line)
;;(global-set-key '[f7] 'planner-create-task-from-buffer)
;;(global-set-key '[S-f7] 'planner-goto-today)
;;(global-set-key '[f8] 'remember)

;;(define-prefix-command 'my-keys)
;;(global-set-key '[f8] 'my-keys)
;;(global-set-key '[f8 c]  'planner-create-task-from-buffer)
;;(global-set-key '[f8 f8] 'remember)
;;(global-set-key '[f8 t] 'planner-goto-today)

(define-prefix-command 'my-keys)
;; (define-key  my-keys "c"         'planner-create-task-from-buffer)
;; (define-key  my-keys "t"         'planner-goto-today)
(define-key  my-keys '[f8]       'remember)
(define-key  my-keys "m"         'gnus)

(define-prefix-command 'my-keys-functions)
(define-key  my-keys-functions  "p"   'jmi/toggle-http-proxy)

(define-key  my-keys "f"         'my-keys-functions)
(define-key  my-keys "t"         'jmi/open-tasks)


(global-set-key '[f8] 'my-keys)


;;; keybindings.el ends here
