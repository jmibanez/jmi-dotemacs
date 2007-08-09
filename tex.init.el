;; TeX support
(setq tex-dvi-view-command
      (if (eq window-system 'x) "xdvi" "dvi2tty * | cat -s"))

