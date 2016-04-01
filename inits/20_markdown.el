(need-package 'markdown-mode)

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdonw\\'" . markdown-mode)))
