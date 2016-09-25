;; (eval-when-compile)
(require 'cl)

(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(package-initialize)

(defun package-install-with-refresh (package)
  (unless (assq package package-alist)
    (package-refresh-contents))
  (unless (package-installed-p package)
    (package-install package)))

(defun require-or-install (package)
  (or (require package nil t)
      (progn
        (package-install-with-refresh package)
        (require package))))

(defun package-bundle (package)
  (require-or-install package))
;; (defun package-bundle (package)
;;   (or (package-installed-p package)
;;       (package-install-with-refresh package)))

(require-or-install 'use-package)

(setq use-package-always-ensure t)
(setq use-package-verbose t)

(require-or-install 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load
 (expand-file-name "inits/" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-ghc-show-info t)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote stack-ghci))
 '(haskell-stylish-on-save t)
 '(package-selected-packages
   (quote
    (yasnippet yaml-mode use-package tuareg smex smartparens scala-mode rainbow-delimiters railscasts-theme popwin markdown-mode init-loader ido-vertical-mode ido-ubiquitous htmlize geiser exec-path-from-shell evil-surround evil-numbers elixir-mode company-ghc cider ac-slime ac-racer))))
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-vertical-first-match-face ((t (:underline (:inherit ido-first-match)))))
 '(proof-locked-face ((t (:background "gray30"))))
 '(proof-queue-face ((((type x) (class color) (background dark)) (:background "darksalmon")))))
