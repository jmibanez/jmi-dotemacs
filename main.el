;;; main.el -- Entry point for all my start files

;;; Commentary:
;;; This drives loading of all .init.el files in ~/.emacs.init

;;; Code:

;; Basic init funs
(defconst jmi/my-emacs-init-path (file-name-directory load-file-name)
  "Directory where all my init files live.")

(defconst jmi/early-init-file (locate-user-emacs-file "early-init.el")
  "Location of early-init file.")

(defconst jmi/early-init-file-source (concat jmi/my-emacs-init-path "000.init.el")
  "Location of early-init source file.")

;; Add pkg for init-only defined packages that we can leverage use-package on
(add-to-list 'load-path (expand-file-name "pkg" jmi/my-emacs-init-path))

(setq custom-file (expand-file-name "001-custom.init.el" jmi/my-emacs-init-path))

;; Ensure that 000.init.el is copied to early-init.el; eval if it's
;; newer or non-existent
(defun jmi/update-early-init ()
  "Update early-init.el and byte-compile it"
  (interactive)
  (copy-file jmi/early-init-file-source jmi/early-init-file t)
  (byte-compile-file jmi/early-init-file))

(unless (not (file-newer-than-file-p jmi/early-init-file-source
                                     jmi/early-init-file))
  (copy-file jmi/early-init-file-source jmi/early-init-file t)
  (load jmi/early-init-file-source nil nil :no-suffix))


(defun jmi/list-init-files (directory)
  "List all .init.el files inside DIRECTORY."
  (if (not (file-exists-p directory))
      '()
    (let (init-files-list
          (current-dir-list (directory-files-and-attributes directory t)))
      (dolist (dir-item current-dir-list init-files-list)
        (if (equal ".init.el" (substring (car dir-item) -8))
            (let ((dir-item-base (substring (car dir-item) 0 -3)))
                (setq init-files-list
                      (cons dir-item-base
                            init-files-list))))))))


;; If we're debugging init, add more logging
(when init-file-debug
  (setopt use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t))


(defun jmi/platform-init-path ()
  "Return path to directory containing platform-specific init files."
  (let* ((platform-dir (symbol-name system-type))
         (sanitized-platform-dir
          (if (s-contains? "/" platform-dir)
              (car (last (s-split "/" platform-dir)))
            platform-dir)))
    (concat jmi/my-emacs-init-path
            sanitized-platform-dir)))


;; Custom :vc-or-local keyword: acts like :vc, but checks for a local
;; checkout first.  If the :local-path directory exists, load from
;; there (equivalent to :load-path); otherwise fall back to :vc.
;;
;; Usage:
;;   :vc-or-local (:url "https://github.com/..." :local-path "~/projects/..." :rev :newest)

(require 'cl-lib)

;; Insert :vc-or-local right after :vc in the processing order.
(unless (memq :vc-or-local use-package-keywords)
  (let ((vc-position (cl-position :vc use-package-keywords)))
    (setq use-package-keywords
          (append (seq-take use-package-keywords (1+ vc-position))
                  '(:vc-or-local)
                  (seq-drop use-package-keywords (1+ vc-position))))))

(defun use-package-normalize/:vc-or-local (name _keyword args)
  "Normalize arguments to the :vc-or-local keyword.
Accepts the same plist as :vc plus a :local-path key.  The
:local-path value is extracted and the remainder is normalized
as for :vc.  Returns (VC-ARG LOCAL-PATH)."
  (let* ((arg (car args))
         (local-path (and (listp arg) (plist-get arg :local-path)))
         ;; Rebuild the plist without :local-path before handing off
         ;; to the standard :vc normalizer.
         (vc-plist (if (and (listp arg) local-path)
                       (cl-loop for (k v) on arg by #'cddr
                                unless (eq k :local-path)
                                nconc (list k v))
                     arg)))
    (list (use-package-normalize/:vc name _keyword (list vc-plist))
          (when local-path (expand-file-name local-path)))))

(defun use-package-handler/:vc-or-local (name _keyword arg rest state)
  "Install or load NAME, preferring a local checkout when available.
If the :local-path directory exists, add it to `load-path'.
Otherwise install via VC as the :vc keyword would."
  (let* ((body (use-package-process-keywords name rest state))
         (vc-arg (car arg))
         (local-path (cadr arg)))
    (if (bound-and-true-p byte-compile-current-file)
        ;; Compile time: resolve immediately.
        (if (and local-path (file-directory-p local-path))
            (add-to-list 'load-path local-path)
          (funcall #'use-package-vc-install vc-arg nil))
      ;; Runtime: emit a conditional so each Emacs session decides.
      (push (if local-path
                `(if (file-directory-p ,local-path)
                     (add-to-list 'load-path ,local-path)
                   (use-package-vc-install ',vc-arg nil))
              `(use-package-vc-install ',vc-arg nil))
            body))
    body))

(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(require 'bind-key)

;; Bind both F8 and C-* to my jump keys; bind C-* first so that F8 is
;; what shows up in help text
(bind-keys :prefix-map jmi/my-jump-keys-map
           :prefix "C-*")
(bind-key "<f8>" 'jmi/my-jump-keys-map nil nil)

;; If we aren't running on at least this version of Emacs, error
(if (< emacs-major-version 30)
    (error "This Emacs version is too old; need to be at least Emacs 30"))

;; If there are any customizations per-machine, per-user, load them
;; first, so that our config can refer to them
(mapc 'load
      (sort (jmi/list-init-files (jmi/platform-init-path))
            'string-lessp))

;; Load all init modules
(mapc 'load
      (seq-remove (lambda (i) (equal "000.init" (substring i -8)))
                  (sort (jmi/list-init-files jmi/my-emacs-init-path)
			'string-lessp)))

(provide 'jmi-dotemacs)

;;; main.el ends here

