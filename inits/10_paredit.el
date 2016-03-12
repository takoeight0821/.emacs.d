(electric-pair-mode 1)

(defun turn-on-paredit (mode)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook #'(lambda () (electric-pair-mode 0) (enable-paredit-mode)))))

(need-package 'paredit)
(use-package paredit
  :config
  (turn-on-paredit 'emacs-lisp-mode))
