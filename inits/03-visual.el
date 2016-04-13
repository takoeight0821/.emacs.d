(show-paren-mode t)

(global-linum-mode 1)
(setq linum-format "%4d")

(setq eol-mnemonic-dos "(CRLF)"
      eol-mnemonic-mac "(CR)"
      eol-mnemonic-unix "(LF)")

(when window-system
  (tool-bar-mode -1)
  (toggle-scroll-bar nil))

;;
;; Japanese font

(defun set-jp-font (font)
  (when (display-graphic-p)
    (set-fontset-font
     (frame-parameter nil 'font)
     'japanese-jisx0208
     `(,font . "iso10646-1"))))

(add-hook 'window-setup-hook
          (lambda ()
            (custom-set-faces
             (if (mac-os-p)
                 '(default ((t (:height 170 :family "Ricty Diminished"))))
               '(default ((t (:height 120 :family "Ricty Diminished"))))))
            (set-jp-font "Ricty Diminished")))

;; color-theme
(require-or-install 'railscasts-theme)

(load-theme 'railscasts t nil)
(setq frame-background-mode 'dark)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
