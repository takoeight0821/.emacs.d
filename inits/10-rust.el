(need-package 'rust-mode)
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(need-packages '(racer ac-racer))

(custom-set-variables
 '(racer-cmd (expand-file-name "~/.cargo/bin/racer"))
 '(racer-rust-src-path (expand-file-name "~/rustc-src-1.7.0/src")))

(eval-after-load "rust-mode" '(require 'racer))
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(defun my/racer-mode-hook ()
  (require 'ac-racer)
  (ac-racer-setup))
(add-hook 'racer-mode-hook 'my/racer-mode-hook)
