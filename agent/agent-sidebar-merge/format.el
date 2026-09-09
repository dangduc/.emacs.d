;;; Format the merged sources -*- lexical-binding: t; -*-
(put 'iter-defun 'lisp-indent-function 'defun)
(put 'merge--fixture 'lisp-indent-function 0)
(dolist (file '("lisp/agent-sidebar.el"
                "agent/agent-sidebar-merge/tests.el"))
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (setq-local indent-tabs-mode nil)
    (check-parens)
    (indent-region (point-min) (point-max))
    (write-region (point-min) (point-max) file nil 'silent)))
