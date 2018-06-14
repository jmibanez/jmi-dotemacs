;;; 000.init.el -- Linux-specific config

;;; Commentary:
;;; This contains early configuration specific to my Emacs environment
;;; on Linux; other .init.el files in this directory will be loaded
;;; after this

;;; Code:

;; Session handling -- Exit Emacs on D-BUS signal from session manager
(defun jmi/dbus-handle-gnome-session-manager-exit (&rest args)
  "Wrapper for kill-emacs to handle session exit from GNOME session manager."
  ;; (desktop-save "~/.emacs.desktop" t) ;; ??
  (message "Session over")
  (kill-emacs 0))

(setq jmi/dbus-handler (dbus-register-signal
                        :session
                        "org.gnome.SessionManager"
                        "/org/gnome/SessionManager" "org.gnome.SessionManager"
                        "SessionOver"
                        'jmi/dbus-handle-gnome-session-manager-exit))



;;; 000.init.el ends here
