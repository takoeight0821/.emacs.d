(need-package 'evil)
(use-package evil
  :init
  (evil-mode 1)
  :config
  ;; https://github.com/timcharper/evil-surround
  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))
  ;; https://github.com/cofi/evil-numbers
  (use-package evil-numbers
    :config
    (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
    (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)))
