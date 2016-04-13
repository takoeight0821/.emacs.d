(package-bundle 'clojure-mode)

(use-package clojure-mode
  :init
  (add-hook 'clojure-mode #'yas-minor-mode)
  (add-hook 'clojure-mode #'subword-mode)
  (add-hook 'clojure-mode 'rainbow-delimiters-mode-enable))

(package-bundle 'cider)
(use-package cider
  :init
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'evil-insert-state)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages nil
        cider-repl-display-in-current-window nil
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t
        cider-repl-display-help-banner nil))
