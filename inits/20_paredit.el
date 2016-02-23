(need-package 'paredit)
(autoload 'paredit "paredit" nil t)

(add-hook 'emacs-lisp-mode-hook
          'enable-paredit-mode)
(add-hook 'lisp-mode-hook
          'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook
          'enable-paredit-mode)
(add-hook 'clojure-mode-hook
          'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)
(add-hook 'geiser-repl-mode-hook 'enable-paredit-mode)

