;;; jmi-autoframe.el -- Automatic per-monitor frame management  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Provides a global minor mode that automatically creates and manages
;;; one Emacs frame per connected monitor.  When monitors are added or
;;; removed (hotplug), the frame set is reconciled automatically via
;;; `display-monitors-changed-functions'.
;;;
;;; Usage:
;;;   (require 'jmi-autoframe)
;;;   (jmi/autoframe-mode 1)
;;;
;;; Or with use-package:
;;;   (use-package jmi-autoframe
;;;     :config (jmi/autoframe-mode 1))

;;; Code:

(require 'cl-lib)

;;; --------------------------------------------------------------------------
;;; Customization

(defgroup jmi/autoframe nil
  "Automatically manage one frame per monitor."
  :group 'frames
  :prefix "jmi/autoframe-")

(defcustom jmi/autoframe-delete-frames-on-disconnect t
  "When non-nil, delete frames whose monitor has disappeared."
  :type 'boolean
  :group 'jmi/autoframe)

(defcustom jmi/autoframe-extra-frame-parameters nil
  "Alist of extra frame parameters to apply to every newly created frame."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'jmi/autoframe)

(defcustom jmi/autoframe-frame-parameters '((fullscreen . maximized))
  "Alist of frame parameters applied to every managed frame.
These parameters are set both when a frame is first created and
whenever its monitor geometry changes.  The default maximizes each
frame on its monitor.  Set to nil to apply no extra parameters."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'jmi/autoframe)

;;; --------------------------------------------------------------------------
;;; Internal state

(defvar jmi/autoframe--monitor-frame-alist nil
  "Alist mapping monitor identity strings to their corresponding frames.
Each entry is (MONITOR-ID . FRAME) where MONITOR-ID is a stable
string derived from the monitor's name attribute (or geometry if
no name is available).")

;;; --------------------------------------------------------------------------
;;; Internal helpers

(defun jmi/autoframe--monitor-id (monitor-attrs)
  "Return a stable identity string for MONITOR-ATTRS.
Uses the `name' attribute when present, otherwise falls back to
the `geometry' attribute formatted as a string."
  (let ((name (cdr (assq 'name monitor-attrs)))
        (geometry (cdr (assq 'geometry monitor-attrs))))
    (if (and name (not (string-empty-p name)))
        name
      (format "%S" geometry))))

(defun jmi/autoframe--workarea (monitor-attrs)
  "Return the workarea list (X Y WIDTH HEIGHT) from MONITOR-ATTRS.
Falls back to `geometry' if `workarea' is absent."
  (or (cdr (assq 'workarea monitor-attrs))
      (cdr (assq 'geometry monitor-attrs))))

(defun jmi/autoframe--has-multiple-frames-p ()
  "Return non-nil if more than one live frame currently exists."
  (> (length (filtered-frame-list #'frame-live-p)) 1))

(defun jmi/autoframe--apply-workarea (frame workarea)
  "Position FRAME onto the monitor described by WORKAREA and apply frame parameters.
WORKAREA is a list (X Y WIDTH HEIGHT) in pixels.  The frame is
first moved to the monitor's top-left corner so that any
subsequent maximize lands on the correct display.
`jmi/autoframe-frame-parameters' is then applied to the frame."
  (when (and frame (frame-live-p frame) workarea)
    (let ((x (nth 0 workarea))
          (y (nth 1 workarea)))
      (set-frame-position frame x y)
      (dolist (param jmi/autoframe-frame-parameters)
        (set-frame-parameter frame (car param) (cdr param))))))

(defun jmi/autoframe--make-frame (monitor-attrs)
  "Create a new frame positioned on the monitor described by MONITOR-ATTRS.
The frame is placed at the monitor's workarea origin and
`jmi/autoframe-frame-parameters' is applied, so that the frame
lands correctly on the target display."
  (let* ((workarea (jmi/autoframe--workarea monitor-attrs))
         (params (append (when workarea
                           `((left . ,(nth 0 workarea))
                             (top  . ,(nth 1 workarea))))
                         jmi/autoframe-frame-parameters
                         jmi/autoframe-extra-frame-parameters)))
    (with-selected-frame (make-frame params)
      (jmi/startup-screen))))

;;; --------------------------------------------------------------------------
;;; Public commands

;;;###autoload
(defun jmi/autoframe-setup ()
  "Create one frame per current monitor, replacing any prior tracked frames.
This unconditionally rebuilds the frame set from the current
monitor list; existing tracked frames are deleted first."
  (interactive)
  ;; Delete all previously tracked frames (but keep at least one frame alive)
  (let ((tracked-frames (mapcar #'cdr jmi/autoframe--monitor-frame-alist)))
    (dolist (frame tracked-frames)
      (when (and (frame-live-p frame)
                 ;; Never delete the sole remaining frame
                 (jmi/autoframe--has-multiple-frames-p))
        (delete-frame frame))))
  (setq jmi/autoframe--monitor-frame-alist nil)

  ;; If there's a single frame that exists, associate it with the
  ;; "default" monitor (i.e. the first monitor)
  (if (= 1 (length (frame-list)))
      (let* ((default-monitor (first (display-monitor-attributes-list)))
             (singleton-frame (first (frame-list)))
             (id (jmi/autoframe--monitor-id default-monitor)))
        (push (cons id singleton-frame) jmi/autoframe--monitor-frame-alist)))

  ;; Create a new frame for each monitor; skip the first monitor in
  ;; the list
  (let ((monitors (display-monitor-attributes-list)))
    (dolist (mon (rest monitors))
      (let* ((id    (jmi/autoframe--monitor-id mon))
             (frame (jmi/autoframe--make-frame mon)))
        (push (cons id frame) jmi/autoframe--monitor-frame-alist)))))

;;;###autoload
(defun jmi/autoframe-sync (&optional terminal)
  "Reconcile the current frame set with the current monitor list.
- New monitors get a fresh frame.
- Disappeared monitors cause their frame to be deleted when
  `jmi/autoframe-delete-frames-on-disconnect' is non-nil.
- Existing monitors have their frame repositioned/resized if the
  workarea has changed.

This function is added to `display-monitors-changed-functions'
when `jmi/autoframe-mode' is enabled."
  (interactive)
  (let ((current-monitors (display-monitor-attributes-list))
        (seen-ids '()))
    ;; Handle new and existing monitors
    (dolist (mon current-monitors)
      (let* ((id       (jmi/autoframe--monitor-id mon))
             (workarea (jmi/autoframe--workarea mon))
             (entry    (assoc id jmi/autoframe--monitor-frame-alist)))
        (push id seen-ids)
        (if entry
            ;; Existing monitor: reposition/resize its frame
            (jmi/autoframe--apply-workarea (cdr entry) workarea)
          ;; New monitor: create a frame and track it
          (let ((frame (jmi/autoframe--make-frame mon)))
            (push (cons id frame) jmi/autoframe--monitor-frame-alist)))))
    ;; Handle disappeared monitors
    (let ((gone-entries
           (cl-remove-if (lambda (entry)
                           (member (car entry) seen-ids))
                         jmi/autoframe--monitor-frame-alist)))
      (dolist (entry gone-entries)
        (when jmi/autoframe-delete-frames-on-disconnect
          (let ((frame (cdr entry)))
            (when (and (frame-live-p frame)
                       (jmi/autoframe--has-multiple-frames-p))
              (delete-frame frame))))
        (setq jmi/autoframe--monitor-frame-alist
              (delq entry jmi/autoframe--monitor-frame-alist))))))

;;; --------------------------------------------------------------------------
;;; Minor mode

(defun jmi/autoframe--enable ()
  "Enable `jmi/autoframe-mode': set up initial frames and install hook."
  (add-hook 'display-monitors-changed-functions #'jmi/autoframe-sync)
  (jmi/autoframe-setup))

(defun jmi/autoframe--disable ()
  "Disable `jmi/autoframe-mode': remove hook and clear tracked state."
  (remove-hook 'display-monitors-changed-functions #'jmi/autoframe-sync)
  (setq jmi/autoframe--monitor-frame-alist nil))

;;;###autoload
(define-minor-mode jmi/autoframe-mode
  "Global minor mode for automatic per-monitor frame management.

When enabled, one Emacs frame is created for each connected
monitor and the frame set is automatically kept in sync when
monitors are added or removed (via `display-monitors-changed-functions')."
  :global t
  :group 'jmi/autoframe
  :lighter " AutoFrame"
  (if jmi/autoframe-mode
      (jmi/autoframe--enable)
    (jmi/autoframe--disable)))

(provide 'jmi-autoframe)

;;; jmi-autoframe.el ends here
