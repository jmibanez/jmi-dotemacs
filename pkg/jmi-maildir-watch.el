;;; jmi-maildir-watch.el --- Trigger Gnus rescans on Maildir filesystem events  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Watches every `new/' subdirectory under `jmi/maildir-watch-root'
;;; with file-notify and triggers a Gnus rescan when mail is created,
;;; deleted, or renamed there.  Lets Gnus pick up mail dropped by an
;;; external delivery process (mbsync, jma, etc.) without waiting
;;; on `gnus-demon' polling.
;;;
;;; Enable `jmi/maildir-watch-mode' to start watching.  Typical use is
;;; to hook the mode toggle to `gnus-started-hook' and
;;; `gnus-exit-gnus-hook' so it tracks Gnus's lifetime.

;;; Code:

(require 'filenotify)

(declare-function gnus-group-get-new-news "gnus-group")
(declare-function gnus-demon-scan-mail "gnus-demon")
(declare-function gnus-demon-scan-news "gnus-demon")

(defgroup jmi-maildir-watch nil
  "Trigger Gnus rescans on Maildir filesystem events."
  :group 'gnus)

(defcustom jmi/maildir-watch-root "~/Maildir/jmibanez.com/"
  "Root directory whose `new/' subdirectories are watched."
  :type 'directory
  :group 'jmi-maildir-watch)

(defvar jmi/maildir-watch--handles nil
  "Active file-notify descriptors for Maildir `new/' subdirectories.")

(defun jmi/maildir-watch--find-new-dirs (root)
  "Return every `new/' subdirectory under ROOT.
Skips Maildir leaf directories (cur, new, tmp) when recursing."
  (let ((root (expand-file-name root))
        results)
    (when (file-directory-p root)
      (dolist (entry (directory-files root t directory-files-no-dot-files-regexp))
        (when (and (file-directory-p entry)
                   (not (member (file-name-nondirectory entry)
                                '("cur" "new" "tmp"))))
          (let ((new-dir (expand-file-name "new" entry)))
            (when (file-directory-p new-dir)
              (push new-dir results)))
          (setq results (nconc (jmi/maildir-watch--find-new-dirs entry) results)))))
    results))

(defun jmi/maildir-watch--callback (event)
  "Trigger a Gnus rescan on a creation/deletion/rename EVENT."
  (let ((action (nth 1 event)))
    (when (memq action '(created deleted renamed))
      (gnus-group-get-new-news)
      (gnus-demon-scan-mail)
      (gnus-demon-scan-news))))

(defun jmi/maildir-watch--unregister ()
  "Remove all active Maildir watchers."
  (dolist (handle jmi/maildir-watch--handles)
    (file-notify-rm-watch handle))
  (setq jmi/maildir-watch--handles nil))

(defun jmi/maildir-watch--register ()
  "Register watchers on every `new/' subdir of `jmi/maildir-watch-root'."
  (jmi/maildir-watch--unregister)
  (dolist (dir (jmi/maildir-watch--find-new-dirs jmi/maildir-watch-root))
    (push (file-notify-add-watch dir '(change)
                                 #'jmi/maildir-watch--callback)
          jmi/maildir-watch--handles)))

;;;###autoload
(define-minor-mode jmi/maildir-watch-mode
  "Watch Maildir `new/' subdirectories and rescan Gnus on changes.
When enabled, registers file-notify watchers on every `new/' directory
under `jmi/maildir-watch-root'.  Mail creation, deletion, or rename in
any watched directory triggers `gnus-group-get-new-news',
`gnus-demon-scan-mail', and `gnus-demon-scan-news'."
  :global t
  :lighter nil
  :group 'jmi-maildir-watch
  (if jmi/maildir-watch-mode
      (jmi/maildir-watch--register)
    (jmi/maildir-watch--unregister)))

(provide 'jmi-maildir-watch)

;;; jmi-maildir-watch.el ends here