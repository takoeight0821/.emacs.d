(eval-when-compile
  (require 'cl))

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
