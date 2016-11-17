(package-bundle 'sml-mode)
(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)

(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))
