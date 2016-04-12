(require-or-install 'popwin)
(popwin-mode 1)

(setq popwin:special-display-config
      (append popwin:special-display-config
              '((dired-mode :position top)
                ("*Shell Command Output*")
                (compilation-mode :noselect t)
                ("*slime-apropos*")
                ("*slime-macroexpansion*")
                ("*slime-description*")
                ("*slime-compilation*" :noselect t)
                ("*slime-xref*")
                (slime-connection-list-mode)
                (slime-repl-mode)
                (sldb-mode :height 20 :stick t))))
