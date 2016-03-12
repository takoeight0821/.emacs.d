(need-packages '(slime ac-slime))
(use-package slime
  :config
  (require 'slime-autoloads)

  (setq inferior-lisp-program "ros -Q run")

  (setq slime-default-lisp 'roswell)
  (setq slime-lisp-implementations
        `((sbcl ("sbcl" "--dynamic-space-size" "2000"))
          (roswell ("ros" "dynamic-space-size=2000" "-Q" "-l" "~/.sbclrc" "run"))))

  (add-hook 'slime-mode-hook
            (lambda ()
              (global-set-key (kbd "C-c s") 'slime-selector)
              (define-key slime-scratch-mode-map (kbd "C-n") 'slime-eval-print-last-expression)
              (define-key slime-scratch-mode-map (kbd "C-j") 'next-line)))
  (add-hook 'slime-repl-mode-hook
            (lambda ()
              (linum-mode 0)
              (define-key slime-repl-mode-map (kbd "C-n") 'slime-repl-newline-and-indent)
              (define-key slime-repl-mode-map (kbd "C-j") 'next-line)
              (define-key slime-repl-mode-map (kbd "M-r") 'helm-for-files)))
  (setq slime-autodoc-use-multiline-p t)

  (setq slime-contribs
        '(slime-fancy slime-banner slime-indentation))
  (slime-setup slime-contribs)
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
  (modify-syntax-entry ?\} "){" lisp-mode-syntax-table)

  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'slime-repl-mode))

  (font-lock-add-keywords 'lisp-mode '(("\\(?:^\\|[^,]\\)\\(@\\(?:\\sw\\|\\s_\\)+\\)" (1 font-lock-comment-face))))
  (font-lock-add-keywords 'lisp-mode '(("\\(?:^\\|^,:]\\)\\(<\\(?:\\sw\\|\\s_\\)+>\\)" (1 font-lock-type-face))))

  (defun set-pretty-patterns (patterns)
    (loop for (glyph . pairs) in patterns do
          (loop for (regexp . major-modes) in pairs do
                (loop for major-mode in major-modes do
                      (let ((major-mode (intern (concat (symbol-name major-mode) "-mode")))
                            (n (if (string-match "\\\\([^?]" regexp) 1 0)))
                        (font-lock-add-keywords major-mode
                                                `((,regexp (0 (prog1 ()
                                                                (compose-region (match-beginning ,n)
                                                                                (match-end ,n)
                                                                                ,glyph)))))))))))

  (set-pretty-patterns
   '((?λ ("\\<lambda\\>" lisp lisp-interaction emacs-lisp scheme))
     (?λ ("\\<function\\>" js2))))

  (turn-on-paredit 'lisp-mode)
  (turn-on-paredit 'slime-lisp-mode)
  (turn-on-paredit 'slime-repl-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'slime-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode))