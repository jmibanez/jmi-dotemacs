;;; jmi-projectile-subprojects.el --- Subproject-aware Projectile commands  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Commands for working with Projectile subprojects (the parts of a
;;; monorepo marked by `projectile-subproject-markers'), while keeping
;;; the owning repository as the Projectile root:
;;;
;;; - `jmi/projectile-find-file-in-project-or-subproject-dwim' finds a
;;;   file in the current subproject, or in the whole project if there
;;;   is none. Outside a project it falls back to the picker below.
;;;
;;; - `jmi/switch-project-or-subproject' picks from the known projects
;;;   and their subprojects in a single list, then finds a file there.
;;;
;;; Both take a prefix arg to refresh the project's file cache first.
;;;
;;; Usage:
;;;
;;;   (use-package jmi-projectile-subprojects
;;;     :ensure nil
;;;     :commands (jmi/projectile-find-file-in-project-or-subproject-dwim)
;;;     :bind (("s-o" . jmi/switch-project-or-subproject)))

;;; Code:

(require 'projectile)

(defun jmi/projectile--scan-subprojects-p (project-root)
  "Return non-nil if PROJECT-ROOT's subprojects should be listed.
Remote projects are skipped, so building the list never opens a
connection. With caching enabled, only projects whose file list is
already cached (in memory or on disk) are scanned, so building the list
never indexes a project either; without caching, every local project is
scanned each time."
  (and (not (file-remote-p project-root))
       (or (not projectile-enable-caching)
           (gethash project-root projectile-projects-cache)
           (and (eq projectile-enable-caching 'persistent)
                (file-exists-p (projectile-project-cache-file project-root))))))

(defvar jmi/projectile--subproject-entries-cache (make-hash-table :test 'equal)
  "Map a project root to (FILES MARKERS . ENTRIES).
ENTRIES are the subproject entries built from FILES, the root's list in
`projectile-projects-cache', and MARKERS, the subproject markers in
effect at the time.")

(defun jmi/projectile--subproject-entries (root label)
  "Return (LABEL ROOT SUBDIR) entries for the subprojects of ROOT.
Scanning a large project like a monorepo can take over a second, so
when Projectile caches ROOT's files the entries are kept until
Projectile replaces that cached list (on re-indexing or invalidation)
or the subproject markers change."
  (let ((cached (gethash root jmi/projectile--subproject-entries-cache))
        (files (gethash root projectile-projects-cache))
        (markers (projectile--subproject-markers)))
    (if (and cached files
             (eq (car cached) files)
             (equal (cadr cached) markers))
        (cddr cached)
      ;; Scan across project types rather than stopping at the first
      ;; workspace manifest.
      (let* ((subdirs (let ((projectile-subproject-functions
                              '(projectile-subprojects-from-scan)))
                        (projectile-project-subprojects root)))
             (entries (mapcar (lambda (subdir)
                                (list (concat label subdir) root subdir))
                              subdirs)))
        ;; Scanning may have just loaded the list from disk.
        (when-let* ((files (gethash root projectile-projects-cache)))
          (puthash root (cons files (cons markers entries))
                   jmi/projectile--subproject-entries-cache))
        entries))))

(defun jmi/projectile--switch-candidates ()
  "Return (LABEL ROOT SUBDIR) entries for known projects and their subprojects.
LABEL is the abbreviated directory, so completion can treat it as a
file. ROOT is expanded, so it shares a cache entry with
`projectile-find-file'; SUBDIR is relative to ROOT, or nil for the whole
project. See `jmi/projectile--scan-subprojects-p' for which projects
have their subprojects listed."
  (let ((projectile-current-project-on-switch 'keep)
        groups)
    (dolist (known (projectile-relevant-known-projects))
      (let ((root (expand-file-name known))
            (label (abbreviate-file-name known)))
        (push (cons (list label root nil)
                    (when (jmi/projectile--scan-subprojects-p root)
                      (jmi/projectile--subproject-entries root label)))
              groups)))
    ;; `append' copies all but the last group, so the cached entries
    ;; are never modified.
    (apply #'append (nreverse groups))))

(defun jmi/projectile-find-file-in-subdir (project-root subdir &optional invalidate-cache)
  "Find a file in PROJECT-ROOT, restricted to SUBDIR.
SUBDIR is relative to PROJECT-ROOT. With INVALIDATE-CACHE, refresh the
project's file cache first."
  (let ((default-directory project-root))
    (projectile-maybe-invalidate-cache invalidate-cache)
    ;; Narrow the project's own listing rather than indexing the
    ;; subdirectory afresh: it's already there, and indexing the
    ;; subdirectory as if it were a project of its own would drop the
    ;; project's ignore rules.
    (let ((files (projectile--restrict-to-subdirs
                  (projectile-project-files project-root) (list subdir))))
      (unless files
        (user-error "No files in %s" (abbreviate-file-name
                                      (expand-file-name subdir project-root))))
      (let ((file (projectile-completing-read
                   "Find file: " files
                   :caller 'projectile-read-file
                   :sort-function (projectile--frecency-sort-function project-root))))
        (find-file (expand-file-name file project-root))
        (run-hooks 'projectile-find-file-hook)))))

;;;###autoload
(defun jmi/switch-project-or-subproject (&optional invalidate-cache)
  "Switch to a known project or an autodetected subproject.
Subprojects retain their owning project as the Projectile root. With a
prefix arg INVALIDATE-CACHE, refresh the target project's file cache
before finding a file in it."
  (interactive "P")
  (let ((entries (jmi/projectile--switch-candidates)))
    (unless entries
      (user-error "No available known projects"))
    (let* ((choice (projectile-completing-read
                    "Project or subproject: " (mapcar #'car entries)
                    ;; Candidates are directories, so Marginalia and
                    ;; Embark can treat them as files.
                    :category 'file))
           (entry (or (assoc choice entries)
                      (user-error "No project selected")))
           (root (nth 1 entry))
           (subdir (nth 2 entry))
           ;; Go through Projectile's own switch so its hooks run,
           ;; the target's dir-locals apply and the most recent
           ;; project is recorded.  The prefix arg goes to the action,
           ;; not the switch, which would open `projectile-dispatch'.
           (projectile-switch-project-action
            (if subdir
                (lambda ()
                  (jmi/projectile-find-file-in-subdir root subdir invalidate-cache))
              (lambda () (projectile-find-file invalidate-cache)))))
      (projectile-switch-project-by-name root))))

;;;###autoload
(defun jmi/projectile-find-file-in-project-or-subproject-dwim (&optional invalidate-cache)
  "Find a file in the current subproject, or in the whole project if none.
Outside a project, pick a project or subproject with
`jmi/switch-project-or-subproject' instead. With a prefix arg
INVALIDATE-CACHE, refresh the project's file cache first."
  (interactive "P")
  (if-let* ((project-root (projectile-project-root)))
      (let ((subproject (ignore-errors (projectile-subproject-root))))
        ;; A marker in the root itself marks the project, not a subproject.
        (if (and subproject (not (file-equal-p subproject project-root)))
            (jmi/projectile-find-file-in-subdir
             project-root (file-relative-name subproject project-root) invalidate-cache)
          (projectile-find-file-dwim invalidate-cache)))
    (jmi/switch-project-or-subproject invalidate-cache)))

(provide 'jmi-projectile-subprojects)

;;; jmi-projectile-subprojects.el ends here
