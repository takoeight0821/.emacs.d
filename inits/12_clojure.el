(mapc #'need-package
      (list 'ac-cider
            'clojure-mode
            'cider
            'rainbow-delimiters
            'slamhound))

(autoload 'clojure-mode "clojure-mode" nil t)
(with-eval-after-load 'cider "cider"
          (require 'slamhound)
          (setq nrepl-hide-special-buffers t))

(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)

(add-to-list 'ac-modes 'cider-mode)
(add-to-list 'ac-modes 'cider-repl-mode)

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(add-hook 'clojure-mode-hook #'enable-paredit-mode)

(eval-after-load 'cider
  (setq cider-show-error-buffer nil))
