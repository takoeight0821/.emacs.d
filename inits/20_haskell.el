(need-packages '(haskell-mode ghc))
(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'rainbow-delimiters-mode)
  :bind (:map haskell-mode-map
              ("C-c C-l" . haskell-process-load-or-reload)
              ("C-c C-z" . haskell-interactive-switch)
              ("C-c C-n C-t" . haskell-process-do-type)
              ("C-c C-n C-i" . haskell-process-do-info)
              ("C-c C-n C-c" . haskell-process-cabal-build)
              ("C-c C-n c" . haskell-process-cabal))
  :config
  (custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-stylish-on-save t)
   '(haskell-process-type 'stack-ghci)
   '(haskell-interactive-popup-errors nil)))

(use-package haskell-cabal)

(autoload 'ghc-init "stack ghc" nil t)
(autoload 'ghc-debug "stack ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
