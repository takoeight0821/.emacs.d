(need-packages '(slime
                 ac-slime
                 rainbow-delimiters
                 paredit))

(cond
 ((file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
  (load (expand-file-name "~/quicklisp/slime-helper.el")))
 ((file-exists-p (expand-file-name "~/.roswell/impls/ALL/ALL/quicklisp/slime-helper.el"))
  (load (expand-file-name "~/.roswell/impls/ALL/ALL/quicklisp/slime-helper.el")))
 (t (require 'slime)))

(require 'slime-autoloads)

(setq inferior-lisp-program "ros -L sbcl -Q run")

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

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
(modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
(modify-syntax-entry ?\} "){" lisp-mode-syntax-table)

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

(defun my-toggle-resplit-slime-window ()
  "Toggle resplit slime window, vertically or horizontally."
  (interactive)
  (when (get-buffer-window "*slime-repl sbcl*" 'visible)
    (with-selected-window (get-buffer-window "*slime-repl sbcl*" 'visible)
      (let ((before-height (window-height)))
        (delete-window)
        (set-window-buffer
         (select-window (if (= (window-height) before-height)
                            (split-window-vertically)
                          (split-window-horizontally)))
         "*slime-repl sbcl*")))))

(global-set-key "\C-cH" 'my-toggle-resplit-slime-window)

(autoload 'rainbow-delimiters "rainbow-delimiters" nil t)
(add-hook 'emacs-lisp-mode-hook
          'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook
          'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook
          'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook
          'rainbow-delimiters-mode)

(autoload 'paredit "paredit" nil t)

(add-hook 'emacs-lisp-mode-hook
          'enable-paredit-mode)
(add-hook 'lisp-mode-hook
          'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook
          'enable-paredit-mode)
(add-hook 'scheme-mode-hook
          'enable-paredit-mode)
(add-hook 'clojure-mode-hook
          'enable-paredit-mode)
