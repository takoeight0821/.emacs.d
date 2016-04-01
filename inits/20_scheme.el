(need-packages
 '(geiser))

(use-package geiser
  :config
  (setq geiser-racket-binary "/usr/local/bin/racket")
  (setq geiser-active-implementations '(racket))
  (setq geiser-repl-read-only-prompt-p nil) ;; Racket REPL上で(read)の入力を取る際に必要
  (turn-on-paredit 'scheme-mode)
  (turn-on-paredit 'geiser-repl-mode))
