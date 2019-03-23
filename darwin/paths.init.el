;;; paths.init.el -- Platform-specific paths

;;; Commentary:
;;; This sets up various vars that point to environment-specific
;;; paths.  For instance, we set up the path to the local Eclipse
;;; location, as well git.

;;; Code:

(setq jmi/eclipse-dir "/Applications/Eclipse JEE.app/Contents/Eclipse/")

(defun jmi/parse-version-in-directory (dir-name)
  "Extract JDK version from DIR-NAME."
  (if (string-match "\\(\\(1.[[:digit:]]\\)\\|\\([[:digit:]]+.[[:digit:]]+\\)\\).[[:digit:]]+\\(_[[:digit:]]+\\)?"
                    dir-name)
      (match-string 1 dir-name)))

(setq jmi/jvm-homes-alist
      (let ((base "/Library/Java/JavaVirtualMachines/"))
        (mapcar (lambda (candidate)
                  (let ((path (concat base candidate "/Contents/Home/"))
                        (version (jmi/parse-version-in-directory candidate)))
                    (cons version path)))
                (directory-files base nil
                                 "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))))

(setq jmi/lombok-jar (expand-file-name "~/lombok/lombok.jar"))
(setq jmi/java-format-settings-file (expand-file-name "~/projects/defaultFormatterProfile.xml"))

(setq jmi/git "/usr/local/bin/git")

(provide 'jmi-init-platform-paths)

;;; paths.init.el ends here
