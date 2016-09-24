(require-or-install 'elixir-mode)
(require-or-install 'alchemist)
(package-bundle 'ac-alchemist)


(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
                 :when '(("SPC" "RET"))
                 :actions '(insert navigate))
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(sp-ruby-def-post-handler)
                 :actions '(insert navigate)))

(add-hook 'elixir-mode-hook 'ac-alchemist-setup)
(add-hook 'alchemist-iex-mode-hook 'ac-alchemist-setup)
(define-key alchemist-iex-mode-map (kbd "T") nil)
