(defvar *emacs-config-directory* (file-name-directory load-file-name))
(add-to-list 'load-path (expand-file-name "site-lisp/" *emacs-config-directory*))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(defun need-package (pack)
  (unless (package-installed-p pack)
    (package-install pack)))
(defun need-packages (pack-list)
  (mapc #'need-package pack-list))

(need-package 'use-package)
(require 'use-package)

(need-package 'init-loader)

(use-package init-loader
  :config
  (setq init-loader-show-log-after-init 'error-only)
  (init-loader-load
   (expand-file-name "inits/" (file-name-directory load-file-name))))

;;; yaml-mode
(need-package 'yaml-mode)
(use-package yaml-mode
  :commands (yaml-mode)
  :mode (("\\.ya?ml$" . yaml-mode)))
