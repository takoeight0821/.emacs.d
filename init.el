(eval-when-compile
  (require 'cl))

(defun windowsp ()
  (eq system-type 'windows-nt))
(defun mac-os-p ()
  (eq system-type 'darwin))
(defun linuxp ()
  (eq system-type 'gnu/linux))

(setq eval-expression-print-level nil)
(setq max-lisp-eval-depth 10000)
(setq gc-cons-threshold (* 10 gc-cons-threshold))

(display-time)

;; load environment value
(load-file (expand-file-name "~/.emacs.d/shellenv.el"))
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))

(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

(require 'package)
(mapc (lambda (pa) (add-to-list 'package-archives pa))
      '(
       ; ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ;; ("org" . "http://orgmode.org/elpa/")
       ; ("marmalade" . "http://marmalade-repo.org/packages/")
       ))
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

(setq-default use-package-always-ensure t)
(setq-default use-package-verbose t)

(prefer-coding-system 'utf-8)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-major-mode 'lisp-mode)

(setq-default tab-width 2
	      indent-tabs-mode nil)

(setq use-dialog-box nil)
(defalias 'message-box 'message)

(setq echo-keystrokes 0.1)

(setq-default x-select-enable-clipboard t)

(when (mac-os-p)
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
	(process-send-string proc text)
	(process-send-eof proc))))
  (setq interprogram-paste-function 'copy-from-osx)
  (setq interprogram-cut-function 'paste-to-osx))

(when (and (not window-system) (linuxp))
  (when (getenv "DISPLAY")
    (defun xclip-cut-function (text &optional push)
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) "xclip" nil 0 nil "-i" "-selection" "clipboard")))
    (defun xclip-paste-function()
      (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
        (unless (string= (car kill-ring) xclip-output)
          xclip-output )))
    (setq interprogram-cut-function 'xclip-cut-function)
    (setq interprogram-paste-function 'xclip-paste-function))
  (require 'mouse)
  (xterm-mouse-mode t))

(defun mouse-scroll-down ()
  (interactive)
  (scroll-down 1))
(defun mouse-scroll-up ()
  (interactive)
  (scroll-up 1))

(global-set-key [mouse-4] 'mouse-scroll-down)
(global-set-key [mouse-5] 'mouse-scroll-up)

(setq scroll-step 1)

;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq-default require-final-newline nil)
(setq require-final-newline nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; Prevent beeping.
(setq ring-bell-function 'ignore)

(setq make-backup-files nil)
(setq auto-save-default nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq vc-follow-symlinks nil)

(package-bundle 'package-utils)

(defun font-big ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (+ (face-attribute 'default :height) 10)))

(defun font-small ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (- (face-attribute 'default :height) 10)))

(global-set-key (kbd "C--") 'font-small)
(global-set-key (kbd "C-+") 'font-big)

(define-prefix-command 'windmove-map)
(global-set-key (kbd "C-q") 'windmove-map)
(define-key windmove-map "h" 'windmove-left)
(define-key windmove-map "j" 'windmove-down)
(define-key windmove-map "k" 'windmove-up)
(define-key windmove-map "l" 'windmove-right)
(define-key windmove-map "0" 'delete-window)
(define-key windmove-map "1" 'delete-other-windows)
(define-key windmove-map "2" 'split-window-vertically)
(define-key windmove-map "3" 'split-window-horizontally)

(defun split-window-conditional ()
  (interactive)
  (if (> (* (window-height) 2) (window-width))
      (split-window-vertically)
    (split-window-horizontally)))
(define-key windmove-map "s" 'split-window-conditional)

(setq electric-indent-mode nil)

(show-paren-mode t)

;; (global-linum-mode 1)
(setq-default linum-format "%4d ")

(setq eol-mnemonic-dos "(CRLF)"
      eol-mnemonic-mac "(CR)"
      eol-mnemonic-unix "(LF)")

(if (mac-os-p)
    (set-face-attribute 'default nil :family "Migu 1M" :height (cond ((mac-os-p) 180)
                                                                   (t 160)))
  (set-face-attribute 'default nil :family "Inconsolata" :height 140))

(package-bundle 'railscasts-theme)
(package-bundle 'spacemacs-theme)
(package-bundle 'zenburn-theme)
(package-bundle 'jazz-theme)
;; (load-theme 'spacemacs-dark t nil)
(load-theme 'railscasts t nil)
(setq frame-background-mode 'dark)

(require-or-install 'rainbow-delimiters)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require-or-install 'paren-face)
(global-paren-face-mode t)

(require 'saveplace)
(setq-default save-place t)

(package-bundle 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(require-or-install 'yasnippet)
(yas-global-mode 1)

(require-or-install 'company)
(setq-default company-idle-delay 0.1
              company-minimum-prefix-length 2
              company-selection-wrap-around t)

(defun add-company-backend (backend)
  (add-to-list 'company-backends backend))
(add-company-backend 'company-yasnippet)

(global-set-key (kbd "C-M-i") 'company-complete)

;; C-n, C-pで補完候補を次/前の候補を選択
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)

;; C-sで絞り込む
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)

;; TABで候補を設定
(define-key company-active-map (kbd "C-i") 'company-complete-selection)

;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

(set-face-attribute 'company-tooltip nil
                    :foreground "black" :background "lightgrey")
(set-face-attribute 'company-tooltip-common nil
                    :foreground "black" :background "lightgrey")
(set-face-attribute 'company-tooltip-common-selection nil
                    :foreground "white" :background "steelblue")
(set-face-attribute 'company-tooltip-selection nil
                    :foreground "black" :background "steelblue")
(set-face-attribute 'company-preview-common nil
                    :background nil :foreground "lightgrey" :underline t)
(set-face-attribute 'company-scrollbar-fg nil
                    :background "orange")
(set-face-attribute 'company-scrollbar-bg nil
                    :background "gray40")
;; (global-set-key "\t" 'company-indent-or-complete-common)

(require-or-install 'auto-complete)
(ac-config-default)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(auto-complete-mode -1)
(company-mode 1)

(defvar *autocompletion-mode* 'auto-complete)
(defun autocompletion-with (mode)
  (message "autocompletion called with %s" mode)
  (auto-complete-mode -1)
  (company-mode -1)
  (if (eq mode 'company)
      (progn (auto-complete-mode -1)
             (company-mode 1)
             (setq *autocompletion-mode* 'company))
    (progn (company-mode -1)
           (auto-complete-mode 1)
           (setq *autocompletion-mode* 'auto-complete))))

(add-hook 'after-init-hook  '(lambda () (autocompletion-with *autocompletion-mode*)))

(mapc #'package-bundle
      '(esup noflet))

(mapc #'require-or-install '(evil evil-surround evil-numbers))

(require 'evil)
(evil-mode 1)

;; https://github.com/timcharper/evil-surround
(global-evil-surround-mode 1)

(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(define-key help-mode-map (kbd "i") 'evil-emacs-state)

;; https://github.com/cofi/evil-numbers
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

(define-key evil-emacs-state-map (kbd "C-j") 'evil-normal-state)

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(setq evil-esc-delay 0)

(eval-after-load "slime"
  '(progn
     (define-key evil-normal-state-map (kbd "M-.") 'slime-edit-definition)
     (define-key evil-normal-state-map (kbd "M-,") 'slime-pop-find-definition-stack)))

(ido-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(ido-everywhere 1)

(setq ido-enable-flex-matching t) ;; 中間/あいまい一致

(package-bundle 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require-or-install 'ido-vertical-mode)
(ido-vertical-mode 1)

(require-or-install 'popwin)
(popwin-mode 1)
(setq popwin:close-popup-window-timer-interval 0.5)
(setq popwin:special-display-config
      (append popwin:special-display-config
              '((dired-mode :position top)
                ("*Shell Command Output*")
                (compilation-mode :noselect t)
                ("*slime-apropos*")
                ("*slime-macroexpansion*")
                ("*slime-description*")
                ("*slime-compilation*" :noselect t)
                ("*slime-xref*")
                ("*cider-error*")
                ("*GHC Error*")
                (slime-connection-list-mode)
                (slime-repl-mode)
                (sldb-mode :height 20 :stick t))))

(package-bundle 'smartparens)
(require 'smartparens-config)
;; (require 'bind-key)
(add-hook 'after-init-hook 'turn-on-smartparens-mode)
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

(define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

(define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

(define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

(define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

(smartparens-global-mode t)
;; (load-file (let ((coding-system-for-read 'utf-8))
;;              (shell-command-to-string "agda-mode locate")))

;; (when (mac-os-p)
;;   (package-bundle 'clojure-mode)
;;   (package-bundle 'cider)

;;   (use-package clojure-mode
;;     :config
;;     (add-hook 'clojure-mode #'yas-minor-mode)
;;     (add-hook 'clojure-mode #'subword-mode)
;;     (add-hook 'clojure-mode #'turn-on-smartparens-strict-mode))

;;   (use-package cider
;;     :config
;;     (add-hook 'cider-mode-hook #'(lambda ()
;;                                    (autocompletion-with 'company)
;;                                    ))
;;     (add-hook 'cider-mode-hook #'eldoc-mode)
;;     (add-hook 'cider-repl-mode-hook #'(lambda ()
;;                                         (autocompletion-with 'company)))
;;     (add-hook 'cider-repl-mode-hook #'turn-on-smartparens-strict-mode)
;;     (add-hook 'cider-repl-mode-hook #'eldoc-mode)
;;     (add-hook 'cider-repl-mode-hook #'evil-insert-state)
;;     (setq nrepl-log-messages nil
;;           cider-repl-display-in-current-window nil
;;           cider-repl-use-clojure-font-lock t
;;           cider-prompt-save-file-on-load 'always-save
;;           cider-font-lock-dynamically '(macro core function var)
;;           cider-overlays-use-font-lock t
;;           cider-repl-display-help-banner nil)))

;; (load (expand-file-name "~/.roswell/helper.el"))
(defun roswell-configdir ()
  (substring (shell-command-to-string "ros roswell-internal-use version confdir") 0 -1))

(defun roswell-opt (var)
  (with-temp-buffer
    (insert-file-contents (concat (roswell-configdir) "config"))
    (goto-char (point-min))
    (re-search-forward (concat "^" var "\t[^\t]+\t\\(.*\\)$"))
    (match-string 1)))

(defun roswell-slime-directory ()
  (concat
   (roswell-configdir)
   "lisp/slime/"
   (roswell-opt "slime.version")
   "/"))

(defvar roswell-slime-contribs '(slime-fancy))

(let* ((slime-directory (roswell-slime-directory)))
  (add-to-list 'load-path slime-directory)
  (require 'slime-autoloads)
  (setq slime-backend (expand-file-name "swank-loader.lisp"
                                        slime-directory))
  (setq slime-path slime-directory)
  (slime-setup roswell-slime-contribs))

(setq inferior-lisp-program "ros run")

(when (eq *autocompletion-mode* 'auto-complete)
  (require-or-install 'ac-slime)
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'slime-repl-mode)))

(when (eq *autocompletion-mode* 'company)
  (package-bundle 'slime-company)
  (slime-setup '(slime-fancy slime-company)))

(unless (or (eq *autocompletion-mode* 'auto-complete) (eq *autocompletion-mode* 'company))
  (slime-setup '(slime-fancy)))

;; (setq inferior-lisp-program "ros -Q run")


(add-hook 'lisp-mode-hook 'turn-on-smartparens-strict-mode)
;; cl21
(add-hook 'slime-connected-hook
          (lambda ()
            (when (slime-eval `(cl:if (cl:find-package :cl21-user) t))
              (slime-repl-set-package :cl21-user)
              (slime-repl-eval-string "(cl21:enable-cl21-syntax)"))) t)

;; (when (mac-os-p)
;;   (autoload 'proof-mode "/usr/local/share/emacs/site-lisp/proof-general/generic/proof-site" nil t)
;;     (add-hook 'proof-mode-hook
;;               '(lambda ()
;;                  (define-key proof-mode-map (kbd "C-c RET") 'proof-goto-point)))
;;     (add-hook 'proof-mode-hook
;;               '(lambda ()
;;                  (define-key proof-mode-map (kbd "C-c RET") 'proof-goto-point)))
;;     (setf proof-splash-enable nil)
;;     (when (not window-system)
;;       (setf proof-colour-locked t)
;;       (setf overlay-arrow-string ""))
;;     (setf proof-follow-mode 'followdown)
;;     (setq coq-prog-name "coqtop")
;;     (custom-set-faces
;;      ;; custom-set-faces was added by Custom.
;;      ;; If you edit it by hand, you could mess it up, so be careful.
;;      ;; Your init file should contain only one such instance.
;;      ;; If there is more than one, they won't work right.
;;      '(proof-locked-face ((t (:background "gray20"))))
;;      '(proof-queue-face ((t (:background "brightred"))))))

(require-or-install 'elixir-mode)
(require-or-install 'alchemist)
;;(require-or-install 'ac-alchemist)


(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
                 :when '(("SPC" "RET"))
                 :actions '(insert navigate))
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(sp-ruby-def-post-handler)
                 :actions '(insert navigate)))

(add-hook 'elixir-mode-hook (lambda () (autocompletion-with 'company)))
(add-hook 'alchemist-iex-mode-hook 'elixir-mode)
(add-hook 'alchemist-mix-mode-hook 'evil-insert-state)
(define-key alchemist-iex-mode-map (kbd "T") nil)

(package-bundle 'erlang)
(require 'erlang-start)
(setq erlang-electric-commands '())
;; (when (mac-os-p)
;;   (setq load-path (cons (expand-file-name "~/.kerl/19.3/lib/tools-2.9.1/emacs") load-path))
;;   (setq erlang-root (expand-file-name "~/.kerl/19.3/lib/erlang"))
;;   (setq erlang-man-root-dir (expand-file-name "~/.kerl/19.3/lib/erlang/man"))
;;   (setq exec-path (cons (expand-file-name "~/.kerl/19.3/bin") exec-path))
;;   (require 'erlang-start)
;;   (setq erlang-electric-commands '()))

;; (when (linuxp)
;;   (setq load-path (cons "/home/yuya/kerl/19.2/lib/tools-2.9/emacs" load-path))
;;   (setq erlang-root "/home/yuya/kerl/19.2/lib/erlang")
;;   (setq erlang-man-root-dir "/home/yuya/kerl/19.2/lib/erlang/man")
;;   (setq exec-path (cons "/home/yuya/kerl/19.2/bin" exec-path))
;;   (require 'erlang-start)
;;   (setq erlang-electric-commands '())
;;   )
(mapc #'require-or-install
      '(haskell-mode ghc hindent company-ghc intero))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'rainbow-delimiters-mode)
(add-hook 'haskell-mode-hook 'intero-mode)
(add-hook 'haskell-mode-hook 'hindent-mode)

(setq
 company-ghc-show-info t
 haskell-interactive-popup-errors t
 haskell-process-auto-import-loaded-modules t
 haskell-process-log t
 haskell-process-suggest-remove-import-lines t
 haskell-process-type (quote stack-ghci)
 haskell-stylish-on-save t
 )

;; (require 'haskell-cabal)

(autoload 'ghc-init "stack ghc" nil t)
(autoload 'ghc-debug "stack ghc" nil t)
(add-hook 'haskell-mode-hook '(lambda () (ghc-init)))

;; (add-hook 'haskell-mode-hook '(lambda () (auto-complete-mode 0)))
(add-hook 'haskell-mode-hook (lambda () (autocompletion-with 'company)))
(add-to-list 'company-backends 'company-ghc)

;; (eval-after-load 'haskell-mode '(progn
;;   (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;;   (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
;;   (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
;; (eval-after-load 'haskell-cabal '(progn
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(require-or-install 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


(require-or-install 'geiser)
(setq geiser-active-implementations '(racket chicken))

(require-or-install 'racket-mode)

(add-hook 'racket-mode-hook
          (lambda ()
            (define-key racket-mode-map (kbd "C-c r") 'racket-run)))

(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)

(package-bundle 'rust-mode)
(require-or-install 'racer)
;;; racerやrustfmt、コンパイラにパスを通す
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
;;; rust-modeでrust-format-on-saveをtにすると自動でrustfmtが走る
(eval-after-load "rust-mode"
  '(progn (setq-default rust-format-on-save t)
          (setq company-tooltip-align-annotations t)))
;;; rustのファイルを編集するときにracerとflycheckを起動する
(add-hook 'rust-mode-hook #'racer-mode)

;;; racerのeldocサポートを使う
(add-hook 'racer-mode-hook #'eldoc-mode)
;;; racerの補完サポートを使う
(add-hook 'racer-mode-hook (lambda () (autocompletion-with 'company)))

(package-bundle 'scala-mode)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(package-bundle 'sml-mode)
(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)

(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))

(add-hook 'sml-mode-hook 'turn-on-smartparens-mode)
(add-hook 'sml-mode-hook 'electric-indent-mode)
(add-hook 'sml-mode-hook (lambda () (autocompletion-with 'autocomplete)))

(package-bundle 'yaml-mode)
(use-package yaml-mode
  :mode "\\.yml\\'"
  :bind ("C-m" . newline-and-indent))

(require-or-install 'go-mode)
(add-hook 'go-mode-hook 'smartparens-mode)
(with-eval-after-load 'go-mode
  (require-or-install 'company-go)
  (require-or-install 'go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook (lambda () (autocompletion-with 'company)))
  (add-company-backend 'company-go)
  (add-hook 'before-save-hook 'gofmt-before-save))

(when (mac-os-p) (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el"))

(require-or-install 'd-mode)
(require-or-install 'ac-dcd)

(add-hook 'd-mode-hook
      (lambda ()
         (autocompletion-with 'auto-complete)
         (when (featurep 'yasnippet) (yas-minor-mode-on))
         (ac-dcd-maybe-start-server)
         (ac-dcd-add-imports)
         (add-to-list 'ac-sources 'ac-source-dcd)
         (define-key d-mode-map (kbd "C-c ?") 'ac-dcd-show-ddoc-with-buffer)
         (define-key d-mode-map (kbd "C-c .") 'ac-dcd-goto-definition)
         (define-key d-mode-map (kbd "C-c ,") 'ac-dcd-goto-def-pop-marker)
         (define-key d-mode-map (kbd "C-c s") 'ac-dcd-search-symbol)

         (when (featurep 'popwin)
           (add-to-list 'popwin:special-display-config
                        `(,ac-dcd-error-buffer-name :noselect t))
           (add-to-list 'popwin:special-display-config
                        `(,ac-dcd-document-buffer-name :position right :width 80))
           (add-to-list 'popwin:special-display-config
                        `(,ac-dcd-search-symbol-buffer-name :position bottom :width 5)))))

(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(add-hook 'prolog-mode-hook (lambda () (autocompletion-with 'auto-complete)))

;; (package-bundle 'ess)
;; (require 'ess-site)
;; (setq ess-use-auto-complete t)
;; (add-hook 'julia-mode-hook #'(lambda () (autocompletion-with 'autocomplete)))
;; (add-hook 'inferior-ess-mode-hook 'evil-insert-state)

(add-hook 'c-mode-hook 'turn-on-smartparens-mode)

;; (add-hook 'after-init-hook 'electric-pair-mode)
;; (add-hook 'after-init-hook 'electric-indent-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (erlang ocp-indent ac-slime zenburn-theme yasnippet yaml-mode use-package spacemacs-theme sml-mode smex smartparens slime-company scala-mode rainbow-delimiters railscasts-theme racket-mode racer popwin paren-face package-utils noflet markdown-mode jazz-theme ido-vertical-mode hydra go-eldoc geiser flycheck evil-surround evil-numbers esup edts company-go company-ghc alchemist))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(proof-locked-face ((t (:background "gray20"))))
 '(proof-queue-face ((t (:background "brightred")))))

;; ;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ;; ## end of OPAM user-setup addition for emacs / base ## keep this line
