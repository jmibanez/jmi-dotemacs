;;; paths.init.el -- Platform-specific paths

;;; Commentary:
;;; This sets up various vars that point to environment-specific
;;; paths.  For instance, we set up the path to the local Eclipse
;;; location, as well git.

;;; Code:

;; These are just filler, as we don't yet have a local Linux
;; installation to test this under

(setq jmi/eclipse-dir "/usr/share/eclipse")
(setq jmi/jvm-homes-alist
      '(("1.8" . "/usr/lib/jvm/openjdk-8")))

(setq jmi/git "/usr/bin/git")

(provide 'jmi-init-platform-paths)

;;; paths.init.el ends here

