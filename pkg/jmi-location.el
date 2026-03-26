;;; jmi-location.el --- Dynamic calendar location  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Populates calendar-latitude, calendar-longitude, and
;;; calendar-location-name from ipinfo.io (IP geolocation).
;;; City-level accuracy is sufficient for hemisphere/solar calendar use.
;;;
;;; Enable `jmi/location-mode' to start periodic updates.
;;; Call `jmi/update-calendar-location' to update immediately.
;;; Existing values are left in place if the lookup fails (fallback).

;;; Code:

(require 'url)
(require 'json)

(defgroup jmi-location nil
  "Dynamic calendar location via IP geolocation."
  :group 'calendar)

(defcustom jmi/location-update-interval 1
  "How often to update calendar location, in hours."
  :type 'number
  :group 'jmi-location)

(defvar jmi/location-ipinfo-url
  "https://ipinfo.io/json"
  "URL for IP-based geolocation.")

(defvar jmi/location--timer nil
  "Timer for periodic location updates; managed by `jmi/location-mode'.")

(defun jmi/location--set (lat lon name)
  "Update calendar location vars.
LAT and LON are numbers; NAME is a string.  Only non-nil values are
applied, so existing (fallback) values are preserved on failure."
  (when (and lat lon)
    (setopt calendar-latitude  lat
            calendar-longitude lon))
  (when (and name (not (string-empty-p name)))
    (setopt calendar-location-name name)))

(defun jmi/location--ipinfo-callback (status)
  "Handle ipinfo.io response and update calendar location."
  (when (null (plist-get status :error))
    (goto-char (point-min))
    (when (re-search-forward "^$" nil t)
      (forward-char)
      (condition-case nil
          (let* ((json-object-type 'plist)
                 (data    (json-read))
                 (loc     (plist-get data :loc))
                 (city    (plist-get data :city))
                 (region  (plist-get data :region))
                 (country (plist-get data :country)))
            (when loc
              (let* ((coords (split-string loc ","))
                     (lat    (string-to-number (car  coords)))
                     (lon    (string-to-number (cadr coords)))
                     (name   (mapconcat #'identity
                                        (seq-filter #'identity
                                                    (list city region country))
                                        ", ")))
                (jmi/location--set lat lon name))))
        (error nil)))))

;;;###autoload
(defun jmi/update-calendar-location ()
  "Update calendar location vars from IP geolocation via ipinfo.io.
Existing values are kept if the lookup fails (e.g. when offline)."
  (interactive)
  (url-retrieve jmi/location-ipinfo-url
                #'jmi/location--ipinfo-callback
                nil :silent))

;;;###autoload
(define-minor-mode jmi/location-mode
  "Periodically update calendar location from IP geolocation.
When enabled, calls `jmi/update-calendar-location' immediately and
then repeats every `jmi/location-update-interval' hours.
Disable to stop polling."
  :global t
  :lighter nil
  :group 'jmi-location
  (if jmi/location-mode
      (let ((interval (* jmi/location-update-interval 3600)))
        (jmi/update-calendar-location)
        (setq jmi/location--timer
              (run-with-timer interval interval #'jmi/update-calendar-location)))
    (when jmi/location--timer
      (cancel-timer jmi/location--timer)
      (setq jmi/location--timer nil))))

(provide 'jmi-location)

;;; jmi-location.el ends here