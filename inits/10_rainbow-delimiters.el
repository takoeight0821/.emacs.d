(need-package 'rainbow-delimiters)
(use-package rainbow-delimiters
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))
