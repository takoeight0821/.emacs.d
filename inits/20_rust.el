(need-packages '(rust-mode racer ac-racer))

(use-package rust-mode
  :mode (("\\.rs\\'" . rust-mode))
  :config
  (electric-pair-mode 1)
  (use-package racer
    :config
    (setq racer-cmd (expand-file-name "/Users/konoyuya/.cargo/bin/racer"))
    (setq racer-rust-src-path (expand-file-name "/Users/konoyuya/rustc-src-1.7.0/src/"))
    (add-hook 'rust-mode-hook #'racer-mode)
    ;; (use-package ac-racer
    ;;   :config
    ;;   (add-hook 'racer-mode-hook #'(lambda () (ac-racer-setup))))
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (use-package company)
    (add-hook 'racer-mode-hook #'company-mode)
    (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
    (setq company-tooltip-align-annotations t)))
