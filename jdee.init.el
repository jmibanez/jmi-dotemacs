;; JDEE initializations

(require 'jde)
(require 'cedet)

;; bsh-jar
(setq bsh-jar "/usr/share/java/bsh.jar")

;; DON'T TIMEOUT IMMEDIATELY -- a minute should be enough for Maven
;; projects, due to our hack of using prj.el files there with a
;; classpath of *all* libraries in ~/.m2/repository
(setq bsh-eval-timeout 60)

;; Plugins
(setq jde-plugins-directory "~/elisp/jde-plugins")

;; Project context switching settings
(setq jde-project-context-switching-enabled-p t)

;; Use ant
(setq jde-build-function '(jde-ant-build))
(setq jde-ant-read-target t)

(setq jde-check-version-flag nil)

;; Needed for flymake support
;; Use ECJ as our compiler server

(require 'jde-eclipse-compiler-server)

(setq jde-compiler
      '(("eclipse java compiler server" "/usr/share/java/ecj.jar")))
(setq jde-ecj-command-line-args
      '("-d" "none" "-source" "1.5" "-target" "1.5" "-bootclasspath" "/usr/lib/jvm/java-6-openjdk/jre/lib/rt.jar" "-deprecation"))
(setq jde-flymake-jikes-app-name "ecj")

;; Our installed JDKs
(setq jde-jdk-registry
      '(("6.0" . "/usr/lib/jvm/java-6-openjdk/")
        ("1.5.0" . "/usr/lib/jvm/java-1.5.0-sun/")
        ("1.4" . "/usr/lib/jvm/java-gcj/")))

(setq jde-jdk '("6.0"))

;; Known library paths
(setq jde-lib-directory-names '("^lib" "^jar" "^java"))

;; FIXME: Maven support still broken
(setq jde-maven-project-file-name "pom.xml")

;; For when running, run VM in -server
(setq jde-run-option-hotspot-type 'server)

;; Turn off "which method" in modeline
(setq jde-which-method-mode nil)

;; XRef
(setq jde-xref-db-base-directory "/home/jmibanez/")


(defun jars-in-below-directory (directory)
  "List the .jar files in DIRECTORY and in its sub-directories."
  ;; Although the function will be used non-interactively,
  ;; it will be easier to test if we make it interactive.
  ;; The directory will have a name such as
  ;;  "/usr/local/share/emacs/21.0.100/lisp/"
  (interactive "DDirectory name: ")
  (let (jar-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (cond
       ;; check to see whether filename ends in `.jar'
       ;; and if so, append its name to a list.
       ((equal ".jar" (substring (car (car current-directory-list)) -4))
        (setq jar-files-list
              (cons (car (car current-directory-list)) jar-files-list)))
       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))
        ;; decide whether to skip or recurse
        (if
            (equal "."
                   (substring (car (car current-directory-list)) -1))
            ;; then do nothing since filename is that of
            ;;   current directory or parent, "." or ".."
            ()
          ;; else descend into the directory and repeat the process
          (setq jar-files-list
                (append
                 (jars-in-below-directory
                  (car (car current-directory-list)))
                 jar-files-list)))))
      ;; move to the next filename in the list; this also
      ;; shortens the list so the while loop eventually comes to an end
      (setq current-directory-list (cdr current-directory-list)))
    ;; return the filenames
    jar-files-list))

(defun jde-find-dired (dir regexp)
  "Find java files in DIR containing a regexp REGEXP and start Dired on output.
The command run (after changing into DIR) is

    find . -type f -and -name \\*.java -exec grep -s -e REGEXP {} \\\; -ls

Thus ARG can also contain additional grep options."
  (interactive "DJde-find-dired (directory): \nsJde-find-dired (grep regexp): ")
  (find-dired dir
              (concat "-type f -and -name \\*.java -exec grep " find-grep-options " -e "
                      (shell-quote-argument regexp)
                      " {} \\\; ")))


(require 'font-lock)
;; FIXME temp hack to get a little better java 1.5 support
(let* ((java-keywords
        (eval-when-compile
          (regexp-opt
           '("catch" "do" "else" "super" "this" "finally" "for" "if"
             ;; Anders Lindgren <****> says these have gone.
             ;; "cast" "byvalue" "future" "generic" "operator" "var"
             ;; "inner" "outer" "rest"
             "implements" "extends" "throws" "instanceof" "new"
             "interface" "return" "switch" "throw" "try" "while"))))
       ;;
       ;; Classes immediately followed by an object name.
       (java-type-names
        `(mapconcat 'identity
                    (cons
                     ,(eval-when-compile
                        (regexp-opt '("boolean" "char" "byte" "short" "int" "long"
                                      "float" "double" "void")))
                     java-font-lock-extra-types)
                    "\\|"))
       (java-type-names-depth `(regexp-opt-depth ,java-type-names))
       ;;
       ;; These are eventually followed by an object name.
       (java-type-specs
        (eval-when-compile
          (regexp-opt
           '("abstract" "const" "final" "synchronized" "transient" "static"
             ;; Anders Lindgren <****> says this has gone.
             ;; "threadsafe"
             "volatile" "public" "private" "protected" "native"
             ;; Carl Manning <caroma@ai.mit.edu> says this is new.
             "strictfp"))))
       )

  (setq java-font-lock-keywords-3
        (append
         (list
          ;; support static import statements
          '("\\<\\(import\\)\\>\\s-+\\(static\\)\\s-+\\(\\sw+\\)"
            (1 font-lock-keyword-face)
            (2 font-lock-keyword-face)
            (3 (if (equal (char-after (match-end 0)) ?\.)
                   'jde-java-font-lock-package-face
                 'font-lock-type-face))
            ("\\=\\.\\(\\sw+\\)" nil nil
             (1 (if (and (equal (char-after (match-end 0)) ?\.)
                         (not (equal (char-after (+ (match-end 0) 1)) ?\*)))
                    'jde-java-font-lock-package-face
                  'font-lock-type-face))))
          )

         java-font-lock-keywords-2

         ;;
         ;; More complicated regexps for more complete highlighting for types.
         ;; We still have to fontify type specifiers individually, as Java is hairy.
         (list
          ;;
          ;; Fontify class names with ellipses
          `(eval .
                 (cons (concat "\\<\\(" ,java-type-names "\\)\\>\\.\\.\\.[^.]")
                       '(1 font-lock-type-face)))
          ;;
          ;; Fontify random types immediately followed by an item or items.
          `(eval .
                 (list (concat "\\<\\(\\(?:" ,java-type-names "\\)"
                               "\\(?:\\(?:<.*>\\)\\|\\>\\)\\(?:\\.\\.\\.\\)?\\)"
                               "\\([ \t]*\\[[ \t]*\\]\\)*"
                               "\\([ \t]*\\sw\\)")
                       ;; Fontify each declaration item.
                       (list 'font-lock-match-c-style-declaration-item-and-skip-to-next
                             ;; Start and finish with point after the type specifier.
                             (list 'goto-char (list 'match-beginning
                                                    (+ ,java-type-names-depth 3)))
                             (list 'goto-char (list 'match-beginning
                                                    (+ ,java-type-names-depth 3)))
                             ;; Fontify as a variable or function name.
                             '(1 (if (match-beginning 2)
                                     font-lock-function-name-face
                                   font-lock-variable-name-face)))))
          ;;
          ;; Fontify those that are eventually followed by an item or items.
          (list (concat "\\<\\(" java-type-specs "\\)\\>"
                        "\\([ \t]+\\sw+\\>"
                        "\\([ \t]*\\[[ \t]*\\]\\)*"
                        "\\)*")
                ;; Fontify each declaration item.
                '(font-lock-match-c-style-declaration-item-and-skip-to-next
                  ;; Start with point after all type specifiers.
                  (goto-char (or (match-beginning 5) (match-end 1)))
                  ;; Finish with point after first type specifier.
                  (goto-char (match-end 1))
                  ;; Fontify as a variable or function name.
                  (1 (if (match-beginning 2)
                         font-lock-function-name-face
                       font-lock-variable-name-face))))))))



;; Project file helper for multi-module Maven projects
(defmacro jmi/load-multi-module-pom (base-path pom-path-list other-variable-setters)
  "Macro to load multi-module projects into JDEE. BASE-PATH is
the path to the root of the multi-module project, POM-PATH-LIST
is a list of paths to the submodule pom.xml (relative to
BASE-PATH). Pass in a list of JDEE variables to set in OTHER-VARIABLE-SETTERS."
  (let ((my-classpath (make-symbol "my-full-classpath"))
        (my-sourcepath (make-symbol "my-sourcepath")))
    `(progn
       (require 'pom-parser)
       (setq ,my-classpath  '("/usr/share/java"))
       (setq ,my-sourcepath  '())
       (mapcar
        (lambda (pom-name)
          (progn
            (message "Reading %s" pom-name)
            (with-pom (concat ,base-path pom-name)
              (pom-set-jde-variables *pom-node*))
            (setq ,my-classpath (append ,my-classpath jde-global-classpath))
            (if (stringp jde-sourcepath)
                (setq ,my-sourcepath (append ,my-sourcepath (list jde-sourcepath)))
              (setq ,my-sourcepath (append ,my-sourcepath jde-sourcepath)))))
        ,pom-path-list)
       (jde-set-variables
        ,@other-variable-setters
        '(jde-global-classpath ,my-classpath)
        '(jde-sourcepath ,my-sourcepath)))))
        

  