(package-bundle 'yaml-mode)
(use-package yaml-mode
  :mode "\\.yml\\'"
  :bind ("C-m" . newline-and-indent))
