(mapc #'package-bundle '(tuareg utop merlin))

(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist)) 
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-use-auto-complete-mode t)
(setq merlin-error-after-save nil)

(setq tuareg-electric-indent -1)
(setq tuareg-in-indent 0)
(setq tuareg-use-smie nil)
(setq tuareg-with-indent 0)

(setq opam-share (substring (shell-command-to-string "opam config var share") 0 -1))
(require 'ocp-indent)
