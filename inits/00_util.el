(defun windowsp ()
  (eq system-type 'windows-nt))
(defun mac-os-p ()
  (eq system-type 'darwin))
(defun linuxp ()
  (eq system-type 'gnu/linux))

(setq eval-expression-print-level nil)
(setq max-lisp-eval-depth 10000)
(when (windowsp)
  (setq path-separator ";"))
(setq exec-path (parse-colon-path (getenv "PATH")))

