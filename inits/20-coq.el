(load "/usr/local/share/emacs/site-lisp/proof-general/generic/proof-site")
(setf proof-splash-enable nil)
(when (not window-system)
  (setf proof-colour-locked t)
  (setf overlay-arrow-string ""))
(setf proof-follow-mode 'followdown)
(add-hook 'proof-mode-hook
  '(lambda ()
     (define-key proof-mode-map (kbd "C-c RET") 'proof-goto-point)))
(setq coq-prog-name "/Users/konoyuya/.opam/system/bin/coqtop")
(custom-set-faces
 '(proof-locked-face ((t (:background "gray20"))))
 '(proof-queue-face ((((type x) (class color) (background dark)) (:background "darksalmon")))))

