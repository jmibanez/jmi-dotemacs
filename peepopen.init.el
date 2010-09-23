(require 'peepopen)
(require 'textmate)
(textmate-mode)

;; Gist: http://gist.github.com/398029

(defun open-peepopen-project (project) (interactive (list (read-directory-name "Peepopen for project: " "~/projects/")))
  (flet ((textmate-project-root () (file-truename project)))
    (peepopen-goto-file-gui)))

(global-set-key [(super o)] 'open-peepopen-project)