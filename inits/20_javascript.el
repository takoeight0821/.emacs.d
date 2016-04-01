(need-packages '(js2-mode tern tern-auto-complete))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :config
  (use-package tern-auto-complete
    :config
    (tern-ac-setup))
  (add-hook 'js2-mode-hook (lambda () (tern-mode t) (electric-pair-mode t))))

