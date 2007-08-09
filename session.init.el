;; Session management
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(add-to-list 'session-globals-exclude 'org-mark-ring)
