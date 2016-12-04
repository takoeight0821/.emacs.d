;; (eval-when-compile)
(require 'cl)

;; load environment value
(load-file (expand-file-name "~/.emacs.d/shellenv.el"))
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))

(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
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
  (or (package-installed-p package)
      (package-install-with-refresh package)))

(require-or-install 'use-package)

(setq use-package-always-ensure t)
(setq use-package-verbose t)

(require-or-install 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load
 (expand-file-name "inits/" user-emacs-directory))

(electric-indent-mode nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-ghc-show-info t)
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "c39ae5721fce3a07a27a685c08e4b856a13780dbc755a569bb4393c932f226d7" default)))
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote stack-ghci))
 '(haskell-stylish-on-save t)
 '(package-selected-packages
   (quote
    (bison-mode yasnippet yaml-mode use-package tuareg smex smartparens scala-mode rainbow-delimiters railscasts-theme popwin markdown-mode init-loader ido-vertical-mode ido-ubiquitous htmlize geiser exec-path-from-shell evil-surround evil-numbers elixir-mode company-ghc cider ac-slime ac-racer))))
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-vertical-first-match-face ((t (:underline (:inherit ido-first-match)))))
 '(proof-locked-face ((t (:background "gray20"))))
 '(proof-queue-face ((((type x) (class color) (background dark)) (:background "darksalmon")))))
