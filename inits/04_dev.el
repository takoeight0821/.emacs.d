;;; auto-complete 
(need-package 'auto-complete)
(ac-config-default)

;;; yasnippet
(need-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode t)

;;; flycheck
(need-package 'flycheck)
(global-flycheck-mode)
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;; yaml-mode
(need-package 'yaml-mode)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(define-key yaml-mode-map "\C-m" 'newline-and-indent)

;;; typescript
(need-package 'typescript-mode)
(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
