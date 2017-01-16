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
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))
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

;; (require-or-install 'init-loader)
;; (setq init-loader-show-log-after-init nil)
;; (init-loader-load
;;  (expand-file-name "inits/" user-emacs-directory))

(prefer-coding-system 'utf-8)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-major-mode 'emacs-lisp-mode)

(setq-default tab-width 2
	      indent-tabs-mode nil)

(setq use-dialog-box nil)
(defalias 'message-box 'message)

(setq echo-keystrokes 0.1)

(setq x-select-enable-clipboard t)

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
(setq linum-format "%4d ")

(setq eol-mnemonic-dos "(CRLF)"
      eol-mnemonic-mac "(CR)"
      eol-mnemonic-unix "(LF)")

(set-face-attribute 'default nil :family "Migu 1M" :height 180)

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

(require-or-install 'yasnippet)
(yas-global-mode t)

(require-or-install 'company)
(setq company-idle-delay 0.1
      company-minimum-prefix-length 2
      company-selection-wrap-around t)

(defun add-company-backend (backend)
  (add-to-list 'company-backends (list backend 'company-dabbrev 'company-yasnippet)))

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
(add-hook 'after-init-hook 'global-company-mode)

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
                (slime-connection-list-mode)
                (slime-repl-mode)
                (sldb-mode :height 20 :stick t))))

(package-bundle 'smartparens)
(require 'smartparens-config)
(require 'bind-key)
(smartparens-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management
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

(bind-key "C-c f" (lambda () (interactive) (sp-beginning-of-sexp 2)) smartparens-mode-map)
(bind-key "C-c b" (lambda () (interactive) (sp-beginning-of-sexp -2)) smartparens-mode-map)

(require-or-install 'hydra)
(bind-key "C-M-s"
          (defhydra smartparens-hydra ()
            "Smartparens"
            ("d" sp-down-sexp "Down")
            ("e" sp-up-sexp "Up")
            ("u" sp-backward-up-sexp "Up")
            ("a" sp-backward-down-sexp "Down")
            ("f" sp-forward-sexp "Forward")
            ("b" sp-backward-sexp "Backward")
            ("k" sp-kill-sexp "Kill" :color blue)
            ("q" nil "Quit" :color blue))
          smartparens-mode-map)

(bind-key "H-t" 'sp-prefix-tag-object smartparens-mode-map)
(bind-key "H-p" 'sp-prefix-pair-object smartparens-mode-map)
(bind-key "H-y" 'sp-prefix-symbol-object smartparens-mode-map)
(bind-key "H-h" 'sp-highlight-current-sexp smartparens-mode-map)
(bind-key "H-e" 'sp-prefix-save-excursion smartparens-mode-map)
(bind-key "H-s c" 'sp-convolute-sexp smartparens-mode-map)
(bind-key "H-s a" 'sp-absorb-sexp smartparens-mode-map)
(bind-key "H-s e" 'sp-emit-sexp smartparens-mode-map)
(bind-key "H-s p" 'sp-add-to-previous-sexp smartparens-mode-map)
(bind-key "H-s n" 'sp-add-to-next-sexp smartparens-mode-map)
(bind-key "H-s j" 'sp-join-sexp smartparens-mode-map)
(bind-key "H-s s" 'sp-split-sexp smartparens-mode-map)
(bind-key "H-s r" 'sp-rewrap-sexp smartparens-mode-map)
(defvar hyp-s-x-map)
(define-prefix-command 'hyp-s-x-map)
(bind-key "H-s x" hyp-s-x-map smartparens-mode-map)
(bind-key "H-s x x" 'sp-extract-before-sexp smartparens-mode-map)
(bind-key "H-s x a" 'sp-extract-after-sexp smartparens-mode-map)
(bind-key "H-s x s" 'sp-swap-enclosing-sexp smartparens-mode-map)

(bind-key "C-x C-t" 'sp-transpose-hybrid-sexp smartparens-mode-map)

(bind-key ";" 'sp-comment emacs-lisp-mode-map)

(bind-key [remap c-electric-backspace] 'sp-backward-delete-char smartparens-strict-mode-map)

;;;;;;;;;;;;;;;;;;
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
(bind-key "C-(" 'sp---wrap-with-40 minibuffer-local-map)

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*"
                 :wrap "C-*"
                 :unless '(sp-point-after-word-p sp-point-at-bol-p)
                 :post-handlers '(("[d1]" "SPC"))
                 :skip-match 'sp--gfm-skip-asterisk)
  (sp-local-pair "**" "**")
  (sp-local-pair "_" "_" :wrap "C-_" :unless '(sp-point-after-word-p)))

(defun sp--gfm-skip-asterisk (ms mb me)
  (save-excursion
    (goto-char mb)
    (save-match-data (looking-at "^\\* "))))

;;; org-mode
(sp-with-modes 'org-mode
  (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
  (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
  (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "«" "»"))

(defun sp--org-skip-asterisk (ms mb me)
  (or (and (= (line-beginning-position) mb)
           (eq 32 (char-after (1+ mb))))
      (and (= (1+ (line-beginning-position)) me)
           (eq 32 (char-after me)))))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil
                 :wrap "C-("
                 :pre-handlers '(my-add-space-before-sexp-insertion)
                 :post-handlers '(my-add-space-after-sexp-insertion)))



(defun my-add-space-after-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (forward-char (sp-get-pair id :cl-l))
      (when (or (eq (char-syntax (following-char)) ?w)
                (looking-at (sp--get-opening-regexp)))
        (insert " ")))))

(defun my-add-space-before-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (backward-char (length id))
      (when (or (eq (char-syntax (preceding-char)) ?w)
                (and (looking-back (sp--get-closing-regexp))
                     (not (eq (char-syntax (preceding-char)) ?'))))
        (insert " ")))))

;;; C++
(sp-with-modes '(malabar-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
(sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                    ("* ||\n[i]" "RET")))


(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(package-bundle 'clojure-mode)
(package-bundle 'cider)

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode #'yas-minor-mode)
  (add-hook 'clojure-mode #'subword-mode)
  (add-hook 'clojure-mode #'turn-on-smartparens-strict-mode))

(use-package cider
  :config
  (add-hook 'cider-mode-hook #'(lambda ()
                                 ;; (auto-complete-mode 0)
                                 (company-mode t)))
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'(lambda ()
                                      ;; (auto-complete-mode 0)
                                      (company-mode t)))
  (add-hook 'cider-repl-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'evil-insert-state)
  (setq nrepl-log-messages nil
        cider-repl-display-in-current-window nil
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t
        cider-repl-display-help-banner nil))

(load (expand-file-name "~/.roswell/helper.el"))
;; (require-or-install 'ac-slime)
(package-bundle 'slime-company)
(slime-setup '(slime-fancy slime-company))
(setq inferior-lisp-program "ros -Q run")

(add-hook 'lisp-mode-hook 'smartparens-strict-mode)
;; cl21
(add-hook 'slime-connected-hook
          (lambda ()
            (slime-company-disable)
            (add-company-backend 'company-slime)
            (when (slime-eval `(cl:if (cl:find-package :cl21-user) t))
              (slime-repl-set-package :cl21-user)
              (slime-repl-eval-string "(cl21:enable-cl21-syntax)"))) t)

(load "/usr/local/share/emacs/site-lisp/proof-general/generic/proof-site")
(setf proof-splash-enable nil)
(when (not window-system)
  (setf proof-colour-locked t)
  (setf overlay-arrow-string ""))
(setf proof-follow-mode 'followdown)
(add-hook 'proof-mode-hook
  '(lambda ()
     (define-key proof-mode-map (kbd "C-c RET") 'proof-goto-point)))
(setq coq-prog-name "coqtop")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(proof-locked-face ((t (:background "gray20"))))
 '(proof-queue-face ((t (:background "brightred")))))

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

;; (add-hook 'elixir-mode-hook '(lambda () (auto-complete-mode 0)))
(add-hook 'elixir-mode-hook 'company-mode)
(add-hook 'alchemist-iex-mode-hook 'elixir-mode)
(add-hook 'alchemist-mix-mode-hook 'evil-insert-state)
(define-key alchemist-iex-mode-map (kbd "T") nil)

(setq load-path (cons "/Users/konoyuya/erlang/19.2/lib/tools-2.9/emacs" load-path))
(setq erlang-root "/Users/konoyuya/erlang/19.2/lib/erlang")
(setq erlang-man-root-dir "/Users/konoyuya/erlang/19.2/lib/erlang/man")
(setq exec-path (cons "/Users/konoyuya/erlang/19.2/bin" exec-path))
(require 'erlang-start)
(setq erlang-electric-commands '())

(mapc #'require-or-install
      '(haskell-mode ghc company-ghc))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'rainbow-delimiters-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-ghc-show-info t)
 '(haskell-interactive-popup-errors t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote stack-ghci))
 '(haskell-stylish-on-save t)
 '(package-selected-packages
   (quote
    (go-eldoc company-go go-mode zenburn-theme yasnippet yaml-mode use-package spacemacs-theme sml-mode smex smartparens slime-company scala-mode rainbow-delimiters railscasts-theme racket-mode popwin paren-face package-utils noflet markdown-mode jazz-theme init-loader ido-vertical-mode hydra helm geiser flycheck evil-surround evil-numbers esup company-ghc cider alchemist ac-racer))))

;; (require 'haskell-cabal)

(autoload 'ghc-init "stack ghc" nil t)
(autoload 'ghc-debug "stack ghc" nil t)
(add-hook 'haskell-mode-hook '(lambda () (ghc-init)))

;; (add-hook 'haskell-mode-hook '(lambda () (auto-complete-mode 0)))
(add-hook 'haskell-mode-hook 'company-mode)
(add-to-list 'company-backends '(company-ghc company-yasnippet company-dabbrev))

(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(require-or-install 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
;;   (when (and opam-share (file-directory-p opam-share))
;;     (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
;;     (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
;;     (autoload 'merlin-mode "merlin" nil t nil)
;;     (add-hook 'tuareg-mode-hook 'merlin-mode t)
;;     (add-hook 'caml-mode-hook 'merlin-mode t)

;;     (setq auto-mode-alist
;;           (append '(("\\.ml[ily]?$" . tuareg-mode)
;;                     ("\\.topml$" . tuareg-mode))
;;                   auto-mode-alist))

;;     (setq tuareg-use-smie nil)
;;     (require 'ocp-indent)

;;     (setq merlin-report-warnings nil)
;;     (require 'merlin-company)
;;     ;; ;; Make company aware of merlin
;;     ;; (with-eval-after-load 'company
;;     ;;   (add-to-list 'company-backends 'merlin-company-backend))
;;   ))

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
(add-hook 'rust-mode-hook (lambda ()
                            (racer-mode)))
;;; racerのeldocサポートを使う
(add-hook 'racer-mode-hook #'eldoc-mode)
;;; racerの補完サポートを使う
(add-hook 'racer-mode-hook #'company-mode)

(package-bundle 'scala-mode)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(package-bundle 'sml-mode)
(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)

(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))

(package-bundle 'yaml-mode)
(use-package yaml-mode
  :mode "\\.yml\\'"
  :bind ("C-m" . newline-and-indent))

(require-or-install 'go-mode)
(with-eval-after-load 'go-mode
  (require-or-install 'company-go)
  (require-or-install 'go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-company-backend 'company-go)
  (add-hook 'before-save-hook 'gofmt-before-save))
