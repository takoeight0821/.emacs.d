(package-bundle 'rust-mode)
(use-package rust-mode
  :mode (("\\.rs\\'" . rust-mode)))

(package-bundle 'racer)
(use-package racer
  :init
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'racer-mode-hook 'company-mode)
  :config
  (setq compnay-tooltip-align-annotations t)
  (setq racer-rust-src-path (expand-file-name "$RUST_SRC_PATH/src/")))
