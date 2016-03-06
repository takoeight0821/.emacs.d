(define-key global-map (kbd "RET") 'newline-and-indent)

(defun font-big ()
  (interactive)
  (set-face-attribute 'default nil :height
		      (+ (face-attribute 'default :height) 10)))

(defun font-small ()
  (interactive)
  (set-face-attribute 'default nil :height
		      (- (face-attribute 'default :height) 10)))

(defvar maxframe-fullscreen-p nil)

(defun toggle-fullscreen ()
  (interactive)
  (cond
   ((and (mac-os-p) (< emacs-major-version 24)) (ns-toggle-fullscreen))
   ((and (mac-os-p) (require-or-install 'maxframe))
    (if maxframe-fullscreen-p (restore-frame) (maximize-frame))
    (setq maxframe-fullscreen-p (not maxframe-fullscreen-p)))
   (t
    (if (frame-parameter nil 'fullscreen)
	(set-frame-parameter nil 'fullscreen nil)
      (set-frame-parameter nil 'fullscreen 'fullboth)))))

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

(define-key minibuffer-local-completion-map (kbd "C-w") 'backward-kill-word)

;;; item2のエスケープシーケンスに対応 
(define-key input-decode-map "\e[1;5A" [C-up])
(define-key input-decode-map "\e[1;5B" [C-down])
(define-key input-decode-map "\e[1;5C" [C-right])
(define-key input-decode-map "\e[1;5D" [C-left])
(define-key input-decode-map "\e[1;6A" [C-S-up])
(define-key input-decode-map "\e[1;6B" [C-S-down])
(define-key input-decode-map "\e[1;6C" [C-S-right])
(define-key input-decode-map "\e[1;6D" [C-S-left])
(define-key input-decode-map "\e[1;8A" [C-M-up])
(define-key input-decode-map "\e[1;8B" [C-M-down])
(define-key input-decode-map "\e[1;8C" [C-M-right])
(define-key input-decode-map "\e[1;8D" [C-M-left])
(define-key input-decode-map "\e[1;9A" [M-up])
(define-key input-decode-map "\e[1;9B" [M-down])
(define-key input-decode-map "\e[1;9C" [M-right])
(define-key input-decode-map "\e[1;9D" [M-left])
(define-key input-decode-map "\e[1;10A" [S-M-up])
(define-key input-decode-map "\e[1;10B" [S-M-down])
(define-key input-decode-map "\e[1;10C" [S-M-right])
(define-key input-decode-map "\e[1;10D" [S-M-left])
