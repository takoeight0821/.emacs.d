(package-bundle 'rust-mode)
(use-package rust-mode
  :mode (("\\.rs\\'" . rust-mode)))

(package-bundle 'racer)
(package-bundle 'ac-racer)
(use-package racer
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'(lambda () (ac-racer-setup)))
  ;; (add-hook 'racer-mode-hook #'company-mode)
  ;; (define-key rust-mode-map (kbd "TAB")
  ;;   #'company-indent-or-complete-common)
  ;; (setq company-tooltip-align-annotations t)
  )
