(require 'flymake)
(setq flymake-allowed-file-name-masks
      '(("\\.cpp\\'" flymake-simple-make-init)
        ("\\.html?\\'" flymake-xml-init)
        ("\\.cs\\'" flymake-simple-make-init)
        ("\\.pl\\'" flymake-perl-init)
        ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
        ("\\.java\\'" flymake-java-ecj-init flymake-java-ecj-cleanup)
        ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
        ("\\.tex\\'" flymake-simple-tex-init)
        ("\\.idl\\'" flymake-simple-make-init)))
(setq flymake-log-level 0)
(setq flymake-start-syntax-check-on-find-file nil)


;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "~/scripts/ruby-check.sh" (list local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          '(lambda ()
	     ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
	     (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
		 (flymake-mode))
	     ))
