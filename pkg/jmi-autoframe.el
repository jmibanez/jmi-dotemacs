;;; jmi-autoframe.el -- Automatic per-monitor frame management  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Provides a global minor mode that automatically creates and manages
;;; one Emacs frame per connected monitor.  When monitors are added or
;;; removed (hotplug, sleep/wake), the frame set is reconciled (after a
;;; short debounce) via `display-monitors-changed-functions'.
;;;
;;; Frames are associated with monitors through the
;;; `jmi/autoframe-monitor' frame parameter, so `frame-list' is the
;;; single source of truth.  When a monitor disappears its frame's window
;;; layout is saved, and restored when the same monitor returns.
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
(require 'seq)

;;; --------------------------------------------------------------------------
;;; Customization

(defgroup jmi/autoframe nil
  "Automatically manage one frame per monitor."
  :group 'frames
  :prefix "jmi/autoframe-")

(defcustom jmi/autoframe-delete-frames-on-disconnect t
  "When non-nil, delete frames whose monitor has disappeared.
The window layout of a deleted frame is saved and restored when the
same monitor is reconnected."
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

(defcustom jmi/autoframe-debounce-delay 1.0
  "Seconds to wait after a monitor change before reconciling frames.
Monitor change notifications arrive in bursts (and can report transient
states, e.g. while resuming from sleep), so reconciliation is delayed
until they settle."
  :type 'number
  :group 'jmi/autoframe)

;;; --------------------------------------------------------------------------
;;; Internal state

(defvar jmi/autoframe--saved-layouts (make-hash-table :test #'equal)
  "Hash table mapping monitor IDs to window states of deleted frames.")

(defvar jmi/autoframe--timer nil
  "Pending debounce timer for `jmi/autoframe--reconcile', if any.")

;;; --------------------------------------------------------------------------
;;; Internal helpers

(defun jmi/autoframe--monitor-id (monitor-attrs)
  "Return a base identity string for MONITOR-ATTRS.
Uses the `name' attribute when present, otherwise falls back to
the `geometry' attribute formatted as a string."
  (let ((name (cdr (assq 'name monitor-attrs)))
        (geometry (cdr (assq 'geometry monitor-attrs))))
    (if (and name (not (string-empty-p name)))
        name
      (format "%S" geometry))))

(defun jmi/autoframe--identify-monitors ()
  "Return an alist (ID . ATTRS) for all current monitors.
Monitors sharing a base ID get a \"#N\" suffix (N >= 2) so that
identical displays remain distinguishable."
  (let ((counts (make-hash-table :test #'equal)))
    (mapcar (lambda (mon)
              (let* ((base (jmi/autoframe--monitor-id mon))
                     (n (puthash base (1+ (gethash base counts 0)) counts)))
                (cons (if (= n 1) base (format "%s#%d" base n)) mon)))
            (display-monitor-attributes-list))))

(defun jmi/autoframe--workarea (monitor-attrs)
  "Return the workarea list (X Y WIDTH HEIGHT) from MONITOR-ATTRS.
Falls back to `geometry' if `workarea' is absent."
  (or (cdr (assq 'workarea monitor-attrs))
      (cdr (assq 'geometry monitor-attrs))))

(defun jmi/autoframe--has-multiple-frames-p ()
  "Return non-nil if more than one live frame currently exists."
  (> (length (filtered-frame-list #'frame-live-p)) 1))

(defun jmi/autoframe--frame-monitor (frame)
  "Return the monitor ID that FRAME is tagged with, or nil."
  (frame-parameter frame 'jmi/autoframe-monitor))

(defun jmi/autoframe--frame-for (id)
  "Return the live frame tagged with monitor ID, or nil."
  (seq-find (lambda (f) (equal (jmi/autoframe--frame-monitor f) id))
            (frame-list)))

(defun jmi/autoframe--untagged-frames ()
  "Return graphical top-level frames not associated with any monitor."
  (seq-filter (lambda (f)
                (and (display-graphic-p f)
                     (null (frame-parent f))
                     (null (jmi/autoframe--frame-monitor f))))
              (frame-list)))

(defun jmi/autoframe--adopt (candidates monitor-attrs)
  "Return the frame in CANDIDATES that sits on MONITOR-ATTRS, or nil."
  (let ((geometry (cdr (assq 'geometry monitor-attrs))))
    (seq-find (lambda (f)
                (equal (cdr (assq 'geometry (frame-monitor-attributes f)))
                       geometry))
              candidates)))

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

(defun jmi/autoframe--save-layout (frame id)
  "Save the window layout of FRAME under monitor ID.
The state keeps live buffer objects, so it only survives within the
current Emacs session."
  (puthash id (window-state-get (frame-root-window frame))
           jmi/autoframe--saved-layouts))

(defun jmi/autoframe--restore-layout (frame id)
  "Restore the layout saved for monitor ID into FRAME.
Return non-nil if a saved layout existed and was restored."
  (when-let* ((state (gethash id jmi/autoframe--saved-layouts)))
    (remhash id jmi/autoframe--saved-layouts)
    (window-state-put state (frame-root-window frame) 'safe)
    t))

(defun jmi/autoframe--make-frame (id monitor-attrs)
  "Create a new frame for monitor ID described by MONITOR-ATTRS.
The frame is tagged with ID at creation, so any re-entrant
reconciliation sees it, and is placed at the monitor's workarea
origin.  A previously saved layout is restored if available;
otherwise the startup screen is shown.  Return the frame."
  (let* ((workarea (jmi/autoframe--workarea monitor-attrs))
         (params (append `((jmi/autoframe-monitor . ,id))
                         (when workarea
                           `((left . ,(nth 0 workarea))
                             (top  . ,(nth 1 workarea))))
                         jmi/autoframe-frame-parameters
                         jmi/autoframe-extra-frame-parameters))
         (new-frame (make-frame params)))
    (with-selected-frame new-frame
      (unless (jmi/autoframe--restore-layout new-frame id)
        (jmi/startup-screen)))
    new-frame))

(defun jmi/autoframe--reconcile ()
  "Make the frame set match the current monitors.
- Each monitor gets a frame: the one already tagged for it, else an
  untagged frame already sitting on it, else a newly created one.
- Frames tagged with vanished monitors have their layout saved and are
  deleted when `jmi/autoframe-delete-frames-on-disconnect' is non-nil.
This is idempotent."
  (setq jmi/autoframe--timer nil)
  (when (display-graphic-p)
    (let* ((monitors (jmi/autoframe--identify-monitors))
           (ids (mapcar #'car monitors))
           (candidates (jmi/autoframe--untagged-frames)))
      (pcase-dolist (`(,id . ,mon) monitors)
        (let ((frame (jmi/autoframe--frame-for id)))
          (unless frame
            (setq frame (jmi/autoframe--adopt candidates mon))
            (if frame
                (progn
                  (setq candidates (delq frame candidates))
                  (set-frame-parameter frame 'jmi/autoframe-monitor id))
              (setq frame (jmi/autoframe--make-frame id mon))))
          (jmi/autoframe--apply-workarea frame (jmi/autoframe--workarea mon))))
      (when jmi/autoframe-delete-frames-on-disconnect
        (dolist (frame (frame-list))
          (let ((id (jmi/autoframe--frame-monitor frame)))
            (when (and id
                       (not (member id ids))
                       (jmi/autoframe--has-multiple-frames-p))
              (jmi/autoframe--save-layout frame id)
              (delete-frame frame))))))))

;;; --------------------------------------------------------------------------
;;; Public commands

;;;###autoload
(defun jmi/autoframe-setup ()
  "Reset monitor tracking and reconcile frames immediately.
All frames are untagged first, so existing frames are re-adopted by the
monitor they sit on instead of being deleted and recreated."
  (interactive)
  (when (timerp jmi/autoframe--timer)
    (cancel-timer jmi/autoframe--timer)
    (setq jmi/autoframe--timer nil))
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'jmi/autoframe-monitor nil))
  (clrhash jmi/autoframe--saved-layouts)
  (jmi/autoframe--reconcile))

;;;###autoload
(defun jmi/autoframe-sync (&optional _terminal)
  "Schedule a debounced reconciliation of frames with monitors.
This function is added to `display-monitors-changed-functions'
when `jmi/autoframe-mode' is enabled."
  (interactive)
  (when (timerp jmi/autoframe--timer)
    (cancel-timer jmi/autoframe--timer))
  (setq jmi/autoframe--timer
        (run-with-timer jmi/autoframe-debounce-delay nil
                        #'jmi/autoframe--reconcile)))

;;; --------------------------------------------------------------------------
;;; Minor mode

(defun jmi/autoframe--enable ()
  "Enable `jmi/autoframe-mode': set up initial frames and install hook."
  (add-hook 'display-monitors-changed-functions #'jmi/autoframe-sync)
  (jmi/autoframe-setup))

(defun jmi/autoframe--disable ()
  "Disable `jmi/autoframe-mode': remove hook and clear tracked state."
  (remove-hook 'display-monitors-changed-functions #'jmi/autoframe-sync)
  (when (timerp jmi/autoframe--timer)
    (cancel-timer jmi/autoframe--timer)
    (setq jmi/autoframe--timer nil))
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'jmi/autoframe-monitor nil))
  (clrhash jmi/autoframe--saved-layouts))

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
