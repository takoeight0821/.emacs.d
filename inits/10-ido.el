(mapc #'require-or-install
      '(ido ido-ubiquitous ido-vertical-mode smex ))

(eval-when-compile
  (require 'cl))

(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
(setq ido-max-directory-size 100000)
(ido-mode t)
(add-to-list 'ido-work-directory-list-ignore-regexps tramp-file-name-regexp)

(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(ido-vertical-mode 1)
(custom-set-faces
 '(ido-vertical-first-match-face ((t (:underline (:inherit ido-first-match))))))

;;; work around the compile-log issue
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(defvar ido-context-switch-command nil)
(ido-ubiquitous-mode)

(defun konix/kill-ring-insert ()
  (interactive)
  (let ((is-last-command-yank (eq last-command 'yank))
        (to_insert (ido-completing-read "Yank : "
                                        (delete-duplicates kill-ring :test #'equal))))
    (cond ((and to_insert is-last-command-yank)
           (let ((before (< (point) (mark t))))
             (if before
                 (funcall (or yank-undo-function 'delete-region) (point) (mark t))
               (funcall (or yank-undo-function 'delete-region) (mark t) (point)))
             (setq this-command 'yank)
             (setq yank-undo-function nil)))
          ((and to_insert (region-active-p))
           ;; the currently highlighted section is to be replaced by the yank
           (delete-region (region-beginning) (region-end))))
    (insert to_insert)))
(global-set-key (kbd "M-y") 'konix/kill-ring-insert)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
