(package-bundle 'ddskk)

(setq skk-tut-file (expand-file-name "~/.emacs.d/SKK.tut"))
(require 'skk)
(global-set-key "\C-x\C-j" 'skk-mode)
