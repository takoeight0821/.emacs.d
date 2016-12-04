;; (mapc #'require-or-install '(tuareg utop merlin))

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; (setq auto-mode-alist
    ;;       (append '(("\\.ml[ily]?$" . tuareg-mode)
    ;;                 ("\\.topml$" . tuareg-mode))
    ;;               auto-mode-alist)) 
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
    (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)
    (setq tuareg-electric-indent -1)
    (setq tuareg-in-indent 0)
    (setq tuareg-use-smie nil)
    (setq tuareg-with-indent 0)
    (require 'ocp-indent)))


;; (require 'caml-mode)
;; (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
;; (setq auto-mode-alist
;;       (append '(("\\.ml[ily]?$" . tuareg-mode)
;;                 ("\\.topml$" . tuareg-mode))
;;               auto-mode-alist)) 
;; (add-hook 'tuareg-mode-hook 'merlin-mode)
;; (setq merlin-use-auto-complete-mode t)
;; (setq merlin-error-after-save nil)
