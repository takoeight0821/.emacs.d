(mapc #'require-or-install
      '(haskell-mode ghc))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'rainbow-delimiters-mode)

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-stylish-on-save t)
 '(haskell-process-type 'stack-ghci)
 '(haskell-interactive-popup-errors nil)))

(require 'haskell-cabal)

(autoload 'ghc-init "stack ghc" nil t)
(autoload 'ghc-debug "stack ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
