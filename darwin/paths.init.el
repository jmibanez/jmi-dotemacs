;;; paths.init.el -- Platform-specific paths

;;; Commentary:
;;; This sets up various vars that point to environment-specific
;;; paths.  For instance, we set up the path to the local Eclipse
;;; location, as well git.

;;; Code:

(setq jmi/eclipse-dir "~/apps/eclipse/Eclipse.app/Contents/Eclipse/")

(setq jmi/jvm-homes-alist
      '(("1.8" . "/Library/Java/JavaVirtualMachines/jdk1.8.0_05.jdk/Contents/Home/")
        ("9"   . "/Library/Java/JavaVirtualMachines/jdk-9.0.1.jdk/Contents/Home/")
        ("10"  . "/Library/Java/JavaVirtualMachines/jdk-10.0.1.jdk/Contents/Home/")))

(setq jmi/git "/usr/local/bin/git")

(provide 'jmi-init-platform-paths)

;;; paths.init.el ends here
