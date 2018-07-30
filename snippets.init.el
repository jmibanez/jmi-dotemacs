;;; snippits.init.el -- YASnippet configuration

;;; Commentary:
;;; Separate YASnippet into a separate config file since I'll be
;;; using it for writing templates as well

;;; Code:

(use-package yasnippet

  :config
  ;; From EmacsWiki:
  ;; https://www.emacswiki.org/emacs/Yasnippet
  (defun shk-yas/helm-prompt (prompt choices &optional display-fn)
    "Use helm to select a snippet. Put this into `yas-prompt-functions.'"
    (interactive)
    (setq display-fn (or display-fn 'identity))
    (if (require 'helm-config)
        (let (tmpsource cands result rmap)
          (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
          (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
          (setq tmpsource
                (list
                 (cons 'name prompt)
                 (cons 'candidates cands)
                 '(action . (("Expand" . (lambda (selection) selection))))
                 ))
          (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
          (if (null result)
              (signal 'quit "user quit!")
            (cdr (assoc result rmap))))
      nil))


  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))

  (add-to-list 'yas-prompt-functions #'shk-yas/helm-prompt)

  :after
  helm jmi-keybindings)

;;; snippets.init.el ends here
