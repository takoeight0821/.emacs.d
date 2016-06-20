(package-bundle 'geiser)

(require 'geiser)
(setq geiser-active-implementations '(racket))

(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode-enable)
