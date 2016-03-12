(need-package 'flycheck)
(use-package flycheck
  :config
  (global-flycheck-mode)
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))))
