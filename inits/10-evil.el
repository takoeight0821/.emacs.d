(mapc #'require-or-install '(evil evil-surround evil-numbers))

(require 'evil)
(evil-mode 1)

;; https://github.com/timcharper/evil-surround
(global-evil-surround-mode 1)

;; https://github.com/cofi/evil-numbers
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

(setcdr evil-insert-state-map nil)
;; but [escape] should switch back to normal state
(define-key evil-insert-state-map [escape] 'evil-normal-state)

(defun evil-eshell ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (eshell))

(evil-ex-define-cmd "esh[ell]" 'evil-eshell)

(eval-after-load "esh-module"
  '(setq eshell-modules-list (delq 'eshell-ls (delq 'eshell-unix eshell-modules-list))))

(evil-mode)
