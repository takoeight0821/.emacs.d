(need-package 'helm)
(use-package helm-config
  :config
  (progn (helm-mode 1)
         (helm-adaptive-mode 1)
         (helm-push-mark-mode 1)
         (setq helm-full-frame nil)
         (global-set-key (kbd "M-x") 'helm-M-x)))
