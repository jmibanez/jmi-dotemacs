;;; jmi-suppress-bufferswitch.el -- Suppress buffer switching in other functions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Some packages want to steal focus and switch to their buffers --
;;; e.g. mbsync and wttrin. Use this package to advise key functions
;;; so that they don't steal focus:
;;;
;;;    (jmi/suppress-buffer-switch 'mbsync-process-filter)
;;;
;;; We do this by cl-letf'ing switch-to-buffer and
;;; switch-to-buffer-other-window with functions that do nothing.

;;; Code:

(require 'cl-lib)

(defun jmi/no-op-switch-buffer (buf))

(defun jmi/no-op-switch-buffer-message (msg)
  (lambda (buf)
    (message msg)))

(defun jmi/advice--suppress-buffer-switch (orig-fn &rest args)
  (cl-letf (((symbol-function 'switch-to-buffer)
             #'jmi/no-op-switch-buffer)
            ((symbol-function 'switch-to-buffer-other-window)
             #'jmi/no-op-switch-buffer))
    (apply orig-fn args)))

(defun jmi/advice--suppress-buffer-switch-with-message (msg)
  (lambda (orig-fn &rest args)
    (cl-letf (((symbol-function 'switch-to-buffer)
               (jmi/no-op-switch-buffer-message msg))
              ((symbol-function 'switch-to-buffer-other-window)
               (jmi/no-op-switch-buffer-message msg)))
      (apply orig-fn args))))

(defun jmi/suppress-buffer-switch (fn-sym-to-suppress)
  (advice-add fn-sym-to-suppress :around #'jmi/advice--suppress-buffer-switch))

(defun jmi/suppress-buffer-switch-message-instead (fn-sym-to-suppress msg)
  (advice-add fn-sym-to-suppress :around (jmi/advice--suppress-buffer-switch-with-message msg)))


(provide 'jmi-suppress-bufferswitch)

;;; jmi-suppress-bufferswitch.el ends here
