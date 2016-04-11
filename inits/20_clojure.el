(mapc #'need-package
      (list 'clojure-mode
            'ac-cider
            'cider))

(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'subword-mode))

(use-package cider
  :init
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (turn-on-paredit 'clojure-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (use-package ac-cider
    :config
    (add-to-list 'ac-modes 'cider-mode)
    (add-to-list 'ac-modes 'cider-repl-mode))
  ; (autoload 'ac-cider "ac-cider" nil t)
  (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
  (add-hook 'cider-mode-hook 'ac-cider-setup)
  (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
  (add-hook 'cider-repl-mode-hook 'evil-insert-state)
  (turn-on-paredit 'cider-repl-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t
        cider-repl-display-help-banner nil
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t)
  )
