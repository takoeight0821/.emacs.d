(load "~/.emacs.d/site-lisp/PG/generic/proof-site")
(setf proof-splash-enable nil)
(when (not window-system)
  (setf proof-colour-locked t)
  (setf overlay-arrow-string ""))
(setf proof-follow-mode 'followdown)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; '(ido-vertical-first-match-face ((t (:underline (:inherit ido-first-match)))))
 '(proof-locked-face ((t (:background "gray30"))))
 '(proof-queue-face ((((type x) (class color) (background dark)) (:background "darksalmon")))))

