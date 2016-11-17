(require-or-install 'elixir-mode)
(require-or-install 'alchemist)
;;(require-or-install 'ac-alchemist)


(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
                 :when '(("SPC" "RET"))
                 :actions '(insert navigate))
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(sp-ruby-def-post-handler)
                 :actions '(insert navigate)))

(add-hook 'elixir-mode-hook '(lambda () (auto-complete-mode 0)))
(add-hook 'elixir-mode-hook 'company-mode)
(add-hook 'alchemist-iex-mode-hook 'elixir-mode)
(add-hook 'alchemist-mix-mode-hook 'evil-insert-state)
(define-key alchemist-iex-mode-map (kbd "T") nil)
