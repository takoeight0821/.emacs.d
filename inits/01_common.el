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

(require 'saveplace)
(setq-default save-place t)

;;; evil
(need-package 'evil)
(evil-mode t)
(define-key evil-insert-state-map (kbd "C-j") 'evil-normal-state)


;;; helm
(need-package 'helm)
(require 'helm-config)
(helm-mode 1)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "C-c i")   'helm-imenu)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)
(define-key global-map (kbd "M-r")     'helm-resume)
(define-key global-map (kbd "C-M-h")   'helm-apropos)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;; Disable helm in some functions
;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
;; (add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(find-tag . nil))

(setq helm-buffer-details-flag nil)

;; Emulate `kill-line' in helm minibuffer
(setq helm-delete-minibuffer-contents-from-point t)
(defadvice helm-delete-minibuffer-contents (before emulate-kill-line activate)
"Emulate `kill-line' in helm minibuffer"
(kill-new (buffer-substring (point) (field-end))))

(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-file-exist activate)
"Execute command only if CANDIDATE exists"
(when (file-exists-p candidate)
    ad-do-it))

(setq helm-ff-fuzzy-matching nil)
(defadvice helm-ff--transform-pattern-for-completion (around my-transform activate)
"Transform the pattern to reflect my intention"
(let* ((pattern (ad-get-arg 0))
        (input-pattern (file-name-nondirectory pattern))
        (dirname (file-name-directory pattern)))
    (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
    (setq ad-return-value
        (concat dirname
                (if (string-match "^\\^" input-pattern)
                    ;; '^' is a pattern for basename
                    ;; and not required because the directory name is prepended
                    (substring input-pattern 1)
                    (concat ".*" input-pattern))))))

(defun helm-buffers-list-pattern-transformer (pattern)
(if (equal pattern "")
    pattern
    (let* ((first-char (substring pattern 0 1))
            (pattern (cond ((equal first-char "*")
                            (concat " " pattern))
                        ((equal first-char "=")
                            (concat "*" (substring pattern 1)))
                        (t
                            pattern))))
    ;; Escape some characters
    (setq pattern (replace-regexp-in-string "\\." "\\\\." pattern))
    (setq pattern (replace-regexp-in-string "\\*" "\\\\*" pattern))
    pattern)))


(unless helm-source-buffers-list
(setq helm-source-buffers-list
        (helm-make-source "Buffers" 'helm-source-buffers)))
(add-to-list 'helm-source-buffers-list
            '(pattern-transformer helm-buffers-list-pattern-transformer))

(defadvice helm-ff-sort-candidates (around no-sort activate)
"Don't sort candidates in a confusing order!"
(setq ad-return-value (ad-get-arg 0)))

;;; popwin
(need-package 'popwin)
(require 'popwin)
(popwin-mode 1)

;; M-x anything
(setq helm-samewindow nil)
(push '("*helm*" :height 20) popwin:special-display-config)

;; M-x dired-jump-other-window
(push '(dired-mode :position top) popwin:special-display-config)

;; M-!
(push "*Shell Command Output*" popwin:special-display-config)

;; M-x compile
(push '(compilation-mode :noselect t) popwin:special-display-config)

;; slime
(push "*slime-apropos*" popwin:special-display-config)
(push "*slime-macroexpansion*" popwin:special-display-config)
(push "*slime-description*" popwin:special-display-config)
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
(push "*slime-xref*" popwin:special-display-config)
(push '(sldb-mode :stick t) popwin:special-display-config)
(push 'slime-repl-mode popwin:special-display-config)
(push 'slime-connection-list-mode popwin:special-display-config)
