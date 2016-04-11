(add-hook 'slime-connected-hook
          (lambda ()
            (when (slime-eval `(cl:if (cl:find-package :cl21-user) t))
              (slime-repl-set-package :cl21-user)
              (slime-repl-eval-string "(cl21:enable-cl21-syntax)"))) t)
