(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; require packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
; (add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; utilities

;;; detect OS
(defun windowsp ()
  (eq system-type 'windows-nt))
(defun mac-os-p ()
  (eq system-type 'darwin))
(defun linuxp ()
  (eq system-type 'gnu/linux))

;;; package management
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

(package-bundle 'package-utils)
(require-or-install 'use-package)

;;; general settings
(set-language-environment 'utf-8)
(setq eval-expression-print-level nil)
(setq max-lisp-eval-depth 10000)
(setq gc-cons-threshold (* 10 gc-cons-threshold))
(setq split-width-threshold 90)
(setq create-lockfiles nil)

(prefer-coding-system 'utf-8)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)
(setq-default tab-width 2
              indent-tabs-mode nil)

(setq use-dialog-box nil)
(defalias 'message-box 'message)

(setq echo-keystrokes 0.1)

(setq-default x-select-enable-clipboard t)

(when (mac-os-p)
  (defun copy-from-osx ()
    (shell-command-to-string "reattach-to-user-namespace pbpaste"))
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "reattach-to-user-namespace" "pbcopy")))
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

(setq-default require-final-newline nil)
(setq require-final-newline nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; Prevent beeping.
(setq ring-bell-function 'ignore)

(setq make-backup-files t)
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))
(setq auto-save-default t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq vc-follow-symlinks nil)

(display-time)

(when (file-exists-p (expand-file-name "~/.emacs.d/shellenv.el"))
  (load-file (expand-file-name "~/.emacs.d/shellenv.el"))
  (dolist (path (reverse (split-string (getenv "PATH") ":")))
    (add-to-list 'exec-path path)))

(when (file-directory-p (expand-file-name "~/.emacs.d/site-lisp"))
 (let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path))))

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
(global-set-key (kbd "C-w") 'windmove-map)
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

(global-linum-mode 1)
(column-number-mode 1)

(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))
(setq-default linum-format "%4d ")

(setq eol-mnemonic-dos "(CRLF)"
      eol-mnemonic-mac "(CR)"
      eol-mnemonic-unix "(LF)")

;; (if (mac-os-p)
;;     (set-face-attribute 'default nil :family "Source Han Code JP L" :height 160 :weight 'light)
;;   (set-face-attribute 'default nil :family "Inconsolata" :height 140))
;; (set-face-attribute 'default nil
;; 		    :family "DejaVu Sans Mono"
;; 		    :height 160
;; 		    :weight 'normal
;; 		    :width  'normal)
;; (set-font "fontset-default"
;; 		  (cons (decode-char 'ucs #x2982)
;; 			(decode-char 'ucs #x2982))
;; 		  "STIX")

(package-bundle 'railscasts-theme)
(load-theme 'railscasts t nil)
(setq frame-background-mode 'dark)

(require-or-install 'paren-face)

(global-paren-face-mode t)

(save-place-mode 1)

(require-or-install 'flycheck)
(package-bundle 'flycheck-popup-tip)

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

(require-or-install 'yasnippet)
(package-bundle 'yasnippet-snippets)
(yas-global-mode 1)

(require-or-install 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq-default company-idle-delay 0.1
              company-minimum-prefix-length 2
              company-selection-wrap-around t)

(add-to-list 'company-backends '(company-capf company-yasnippet company-dabbrev))

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

(setq evil-want-abbrev-expand-on-insert-exit nil)
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

;; leader key
(defvar leader-key-map (make-sparse-keymap)
  "Keymap for \"leader key\" shortcuts.")

(define-key evil-normal-state-map "," leader-key-map)
(define-key leader-key-map "b" 'list-buffers)

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
                ;; ("*GHC Error*")
                (slime-connection-list-mode)
                (slime-repl-mode)
                (sldb-mode :height 20 :stick t)
                )))

(package-bundle 'smartparens)
(require 'smartparens-config)
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

;; C/C++
(mapc #'package-bundle '(irony flycheck-irony company-irony irony-eldoc))

(sp-with-modes '(c-mode malabar-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))
(require-or-install 'cc-mode)
(add-hook 'c-mode-common-hook
          (lambda ()
            (turn-on-smartparens-mode)
            (electric-indent-mode 1)
            (company-mode 1)
            (setq c-default-style "k&r")
            (setq indent-tabs-mode nil)
            (setq c-basic-offset 2)))

(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)

(add-hook 'c-mode-hook
          '(lambda()
             (setq-default sp-escape-quotes-after-insert nil)))

(with-eval-after-load "irony"
  ;; (custom-set-variables '(irony-additional-clang-options '("-std=c++14")))
  (add-to-list 'company-backends 'company-irony)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook #'irony-eldoc)
  )

(with-eval-after-load "flycheck"
  (when (locate-library "flycheck-irony")
    (flycheck-irony-setup)))

;; ProofGeneral, Coq
(package-bundle 'proof-general)
(package-bundle 'company-coq)
(add-hook 'coq-mode-hook 'company-mode)
(add-hook 'coq-mode-hook
          #'company-coq-mode)
(add-hook 'proof-mode-hook
          '(lambda ()
             (define-key proof-mode-map (kbd "C-c RET") 'proof-goto-point)))
(add-hook 'proof-mode-hook
          '(lambda ()
             (define-key proof-mode-map (kbd "C-c RET") 'proof-goto-point)))
(setf proof-splash-enable nil)
(when (not window-system)
  (setf proof-colour-locked t)
  (setf overlay-arrow-string ""))
(setf proof-follow-mode 'followdown)
(setq coq-prog-name "coqtop")
(setq coq-compile-before-require t)
(setq proof-three-window-mode-policy 'hybrid)

;; LSP
(package-bundle 'eglot)

;; Haskell
(package-bundle 'haskell-mode)
(package-bundle 'flycheck-haskell)
(require 'haskell-mode)
(require 'floskell)
(add-hook 'haskell-mode-hook 'flycheck-mode)
(add-hook 'haskell-mode-hook #'floskell-mode)

;; (add-hook 'haskell-mode-hook #'eglot-ensure)
;; (with-eval-after-load 'haskell-mode
;;   (setq flymake-allowed-file-name-masks (delete '("\\.l?hs\\'" haskell-flymake-init) flymake-allowed-file-name-masks)))
(add-hook 'haskell-mode-hook 'company-mode)
(package-bundle 'intero)
(require 'intero)
(flycheck-add-next-checker 'intero '(warning . haskell-hlint))
(intero-global-mode 1)

(flycheck-add-next-checker 'haskell-stack-ghc '(warning . haskell-hlint))

;; Markdown
(require-or-install 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; TOML
(require-or-install 'toml-mode)

;; Rust
(package-bundle 'rust-mode)
(require-or-install 'racer)
(package-bundle 'flycheck-rust)

;;; racerやrustfmt、コンパイラにパスを通す
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
;;; rust-modeでrust-format-on-saveをtにすると自動でrustfmtが走る
(with-eval-after-load "rust-mode"
  (setq-default rust-format-on-save t)
  (setq company-tooltip-align-annotations t))
;;; rustのファイルを編集するときにracerとflycheckを起動する
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'flycheck-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;;; racerのeldocサポートを使う
(add-hook 'racer-mode-hook #'eldoc-mode)

;; ;;; racerの補完サポートを使う
(add-hook 'racer-mode-hook 'company-mode)

;; Scala
(package-bundle 'scala-mode)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

;; SML
(package-bundle 'sml-mode)
(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)

(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))

(add-hook 'sml-mode-hook 'turn-on-smartparens-mode)
(add-hook 'sml-mode-hook 'electric-indent-mode)
(add-hook 'sml-mode-hook 'company-mode)

;; Crystal
(package-bundle 'crystal-mode)
(use-package crystal-mode
  :mode "\\.cr\\'")
(add-hook 'crystal-mode-hook 'company-mode)

;; YAML
(package-bundle 'yaml-mode)
(use-package yaml-mode
  :mode "\\.yml\\'"
  :bind ("C-m" . newline-and-indent))

;; Go
(require-or-install 'go-mode)
(add-hook 'go-mode-hook 'smartparens-mode)
(with-eval-after-load 'go-mode
  (require-or-install 'go-eldoc)
  ;; (require-or-install 'go-autocomplete)
  ;; (add-hook 'go-mode-hook '(lambda () (autocompletion-with 'autocomplete)))
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save))

;; Prolog
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.swi\\'" . prolog-mode))

(add-hook 'prolog-mode-hook 'company-mode)

;; LLVM
(when (require 'llvm-mode nil :noerror)
  (c-add-style "llvm.org"
               '("gnu"
                 (fill-column . 80)
                 (c++-indent-level . 2)
                 (c-basic-offset . 2)
                 (indent-tabs-mode . nil)
                 (c-offsets-alist . ((arglist-intro . ++)
                                     (innamespace . 0)
				                             (member-init-intro . ++)))))

  (add-hook 'c-mode-common-hook
            (function
             (lambda nil
               (if (string-match "llvm" buffer-file-name)
                   (progn
		                 (c-set-style "llvm.org")))))))

;; Racket
(require-or-install 'racket-mode)

(add-hook 'racket-mode-hook
          (lambda ()
            (define-key racket-mode-map (kbd "C-c r") 'racket-run)))

(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
(require-or-install 'geiser)
(setq geiser-active-implementations '(racket))
(add-hook 'scheme-mode-hook 'company-mode)

;; Clojure

(package-bundle 'clojure-mode)
(use-package cider
  :ensure t)

;; TypeScript
(use-package typescript-mode
  :ensure t)
(add-hook 'typescript-mode-hook 'company-mode)
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))


;; ;; Agda
;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))

;; Common Lisp

(package-bundle 'slime-company)
(when (file-exists-p (expand-file-name "~/.roswell/helper.el"))
  (load (expand-file-name "~/.roswell/helper.el")))
(setq inferior-lisp-program "ros -Q run")
(add-hook 'lisp-mode-hook 'company-mode)

;; Ruby

(package-bundle 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))
(add-hook 'ruby-mode-hook 'company-mode)

;; esup
(package-bundle 'esup)

;; Shell
(require-or-install 'multi-term)
;; ;; シェルの設定
(defun search-shell ()
  (or (executable-find "zsh")
      (executable-find "bash")
      (error "can't find 'shell' command in PATH!!")))

(setq shell-file-name (search-shell))
(setenv "SHELL" shell-file-name)
(setq multi-term-program shell-file-name)

;; Elm
(require-or-install 'elm-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(proof-locked-face ((t (:background "gray20"))))
 '(proof-queue-face ((t (:background "brightred")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-ignored-server-capabilites (quote (:hoverProvider)))
 '(package-selected-packages
   (quote
    (elm-mode multi-term esup robe slime-company tide typescript-mode cider clojure-mode crystal-mode geiser racket-mode go-eldoc go-mode yaml-mode sml-mode use-package yasnippet toml-mode smex smartparens scala-mode railscasts-theme racer popwin paren-face markdown-mode irony-eldoc ido-vertical-mode hindent flycheck-rust flycheck-popup-tip flycheck-irony flycheck-haskell evil-surround evil-numbers company-irony))))

;; OCaml

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
