(need-package 'rainbow-delimiters)
(autoload 'rainbow-delimiters "rainbow-delimiters" nil t)
(add-hook 'emacs-lisp-mode-hook
          'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook
          'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook
          'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook
          'rainbow-delimiters-mode)
(add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode)
