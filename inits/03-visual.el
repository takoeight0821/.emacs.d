(show-paren-mode t)

(global-linum-mode 1)
(setq linum-format "%4d ")

(setq eol-mnemonic-dos "(CRLF)"
      eol-mnemonic-mac "(CR)"
      eol-mnemonic-unix "(LF)")

(when window-system
  ;; (tool-bar-mode -1)
  (toggle-scroll-bar nil))

;;
;; Japanese font

(defun set-jp-font (font)
  (when (display-graphic-p)
    (set-fontset-font
     (frame-parameter nil 'font)
     'japanese-jisx0208
     `(,font . "iso10646-1"))))

;; (add-hook 'window-setup-hook
;;           (lambda ()
;;             (custom-set-faces
;;              (if (mac-os-p)
;;                  '(default ((t (:height 180 :family "migu-1m-regular"))))
;;                '(default ((t (:height 120 :family "migu-1m-regular"))))))
;;             (set-jp-font "migu-1m-regular")))

(set-frame-font "-*-Migu 1M-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1")
;; color-theme
(require-or-install 'railscasts-theme)


(load-theme 'railscasts t nil)
(setq frame-background-mode 'dark)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
