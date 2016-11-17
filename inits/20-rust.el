(require-or-install 'rust-mode)
(require-or-install 'racer)
(require-or-install 'ac-racer)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'(lambda () (ac-racer-setup)))