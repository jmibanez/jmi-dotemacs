;;; paths.init.el -- Platform-specific paths

;;; Commentary:
;;; This sets up various vars that point to environment-specific
;;; paths.  For instance, we set up the path to the local Eclipse
;;; location, as well git.

;;; Code:

;; These are just filler, as we don't yet have a local Linux
;; installation to test this under

(setq jmi/eclipse-dir "/usr/share/eclipse")

(defun jmi/parse-version-in-directory (dir-name)
  "Extract JDK version from DIR-NAME."

  (cond ((string-match "\\(\\(1.[[:digit:]]\\)\\|\\([[:digit:]]+.[[:digit:]]+\\)\\).[[:digit:]]+\\(_[[:digit:]]+\\)?" dir-name)
         (match-string 1 dir-name))

        ((string-match "amazon-corretto-\\([[:digit:]]+\\).jdk" dir-name)
         (format "%s.0" (match-string 1 dir-name)))))


(setq jmi/jvm-homes-alist
      (let ((base "/usr/lib/jvm/"))
        (cl-remove-if (lambda (tup)
                        (not (car tup)))
                      (mapcar (lambda (candidate)
                                (let ((path (concat base candidate))
                                      (version (jmi/parse-version-in-directory candidate)))
                                  (cons version path)))
                              (directory-files base nil
                                               "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))))

;; (setq jmi/jvm-homes-alist
;;       '(("1.8" . "/usr/lib/jvm/openjdk-8")))

(setq jmi/lombok-jar (expand-file-name "~/lombok/lombok.jar"))
(setq jmi/java-format-settings-file (expand-file-name "~/projects/defaultFormatterProfile.xml"))

(setq jmi/git "/usr/bin/git")

(provide 'jmi-init-platform-paths)

;;; paths.init.el ends here

