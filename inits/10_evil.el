(need-package 'evil)
(use-package evil
  :init
  (evil-mode 1)
  :config
  (define-key evil-insert-state-map (kbd "C-j") 'evil-normal-state))
