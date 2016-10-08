(require-or-install 'geiser)
(setq geiser-active-implementations '(racket))

(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode-enable)
