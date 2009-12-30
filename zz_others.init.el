;; For pymacs-- am using it for developing Ozone

(setq jmibanez-pymacs-dir (expand-file-name "~/pymacs/"))

(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path jmibanez-pymacs-dir))

(defun jmlisp-maybe-pymacs-reload ()
  (let ((pymacsdir jmibanez-pymacs-dir))
        (when (and (string-equal (file-name-directory buffer-file-name)
                                 pymacsdir)
                   (string-match "\\.py\\'" buffer-file-name))
          (pymacs-load (substring buffer-file-name 0 -3)))))

(add-hook 'after-save-hook 'jmlisp-maybe-pymacs-reload)


;; Other init
(require 'ljupdate)
(require 'git)
(require 'vc-git)
(add-to-list 'vc-handled-backends 'git)



;;(require 'planner)
;;(require 'planner-notes-index)
;;(require 'planner-rss)
;;(require 'planner-calendar)
;;(require 'planner-diary)
;;(require 'planner-cyclic)

;;(global-font-lock-mode)
;; (auto-load-library psvn)
;; (auto-load-library vc-svn)

;; Annoyance: Fix to 'Buffer %s not set up for parsing' stuff
;; (setq global-senator-minor-mode t)

;; Load longlines
;;(require 'longlines)

;; Extended page handling
(require 'page-ext)
(put 'narrow-to-page 'disabled nil)

;; Turn on iswitchb
(iswitchb-mode 1)

;; (mouse-wheel-mode)
;; (setq pop-up-windows nil)

;; F10 pipe the message as spam
;; (global-set-key [(f10)]
;;   '(lambda () (interactive) (shell-command "sa-learn --spam --single ")))
;; 
;; ;; F9 pipe the message as ham
;; (global-set-key [(f9)]
;;   '(lambda () (interactive) (shell-command "sa-learn --ham --single ")))

;;(require 'java-mode-indent-annotations)
;;(add-hook 'jde-mode-hook #'java-mode-indent-annotations-setup)


(setq g-user-email "jmibanez@gmail.com")

(require 'flymake)
(add-hook 'find-file-hooks 'flymake-find-file-hook)
(require 'jde-eclipse-compiler-server)
;; (jde-flymake-jikes-app-name)

(setq midnight-mode t)


;; Tramp customizations
(require 'tramp)
(setq tramp-default-method "scp")

;; mailto: links
;; courtesy of John Sullivan http://journal.wjsullivan.net/185095.html
(defun jmi/mailto (url)
  "Follow a mailto URL, prompting for a posting style."
  (let ((gnus-newsgroup-name
         (completing-read "Use posting style of group: "
                          gnus-active-hashtb nil
                          (gnus-read-active-file-p))))
    (setq url (url-unhex-string url))
    (browse-url-mail url))
  ;; message-mail does not do anything with the body argument, so we have to.
  (if (string-match (regexp-quote "?") url)
      (let* ((start (match-end 0))
             (args (url-parse-query-string
                    (substring url start nil)))
             (body (cadr (assoc-string "body" args t))))
        (when body
          (switch-to-buffer (car (message-buffers)))
          (save-excursion
            (message-goto-body)
            (insert body))))))


;; Disable auto-develock as it's irritating.
(setq develock-auto-enable nil)
