;;; paths.init.el -- Platform-specific paths

;;; Commentary:
;;; This sets up various vars that point to environment-specific
;;; paths.  For instance, we set up the path to the local Eclipse
;;; location, as well git.

;;; Code:

(setq jmi/homebrew-location
      (or (and (file-directory-p "/usr/local/Cellar")
               "/usr/local/Cellar")
          "/opt/homebrew"))

(setq jmi/homebrew-binary-path
      (or (and (file-directory-p "/usr/local/Cellar")
               "/usr/local/bin/")
          "/opt/homebrew/bin/"))

(setq jmi/eclipse-dir "/Applications/Eclipse JEE.app/Contents/Eclipse/")

(defun jmi/parse-version-in-directory (dir-name)
  "Extract JDK version from DIR-NAME."

  (cond ((string-match "\\(\\(1.[[:digit:]]\\)\\|\\([[:digit:]]+.[[:digit:]]+\\)\\).[[:digit:]]+\\(_[[:digit:]]+\\)?" dir-name)
         (match-string 1 dir-name))

        ((string-match "amazon-corretto-\\([[:digit:]]+\\).jdk" dir-name)
         (format "%s.0" (match-string 1 dir-name)))))

(setq jmi/jvm-homes-alist
      (let ((base "/Library/Java/JavaVirtualMachines/"))
        (mapcar (lambda (candidate)
                  (let ((path (concat base candidate "/Contents/Home/"))
                        (version (jmi/parse-version-in-directory candidate)))
                    (cons version path)))
                (directory-files base nil
                                 "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))))

(setq jmi/lombok-jar (expand-file-name "~/lombok/lombok.jar"))
(setq jmi/java-format-settings-file (expand-file-name "~/projects/intellijCompatFormatterProfile.xml"))

(setq jmi/git (concat jmi/homebrew-binary-path "git"))

(provide 'jmi-init-platform-paths)

;;; paths.init.el ends here
