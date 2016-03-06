(need-package 'paredit)
(autoload 'paredit "paredit" nil t)

(defun turn-on-paredit ()
  (electric-pair-mode 0)
  (enable-paredit-mode)
  (define-key paredit-mode-map (kbd "M-[") nil))

(add-hook 'emacs-lisp-mode-hook
          'turn-on-paredit)
(add-hook 'lisp-mode-hook
          'turn-on-paredit)
(add-hook 'slime-repl-mode-hook
          'turn-on-paredit)
(add-hook 'clojure-mode-hook
          'turn-on-paredit)
(add-hook 'scheme-mode-hook 'turn-on-paredit)
(add-hook 'geiser-repl-mode-hook 'turn-on-paredit)

