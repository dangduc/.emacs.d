;;; Restore unchanged forms' formatting -*- lexical-binding: t; -*-
(let ((originals (make-hash-table :test 'equal)) parsed)
  (with-temp-buffer
    (insert-file-contents "agent/agent-sidebar-merge/core.el")
    (emacs-lisp-mode)
    (goto-char (point-min))
    (condition-case nil
        (while t
          (forward-comment (point-max))
          (let* ((start (point)) (form (read (current-buffer))))
            (puthash form (buffer-substring-no-properties start (point)) originals)))
      (end-of-file nil)))
  (with-temp-buffer
    (insert-file-contents "lisp/agent-shell-sidebar.el")
    (emacs-lisp-mode)
    (goto-char (point-min))
    (condition-case nil
        (while t
          (forward-comment (point-max))
          (let* ((start (point)) (form (read (current-buffer))) (end (point))
                 (original (gethash form originals)))
            (push form parsed)
            (when original (delete-region start end) (insert original))))
      (end-of-file nil))
    (untabify (point-min) (point-max))
    (let (verified)
      (goto-char (point-min))
      (condition-case nil (while t (push (read (current-buffer)) verified)) (end-of-file nil))
      (unless (equal parsed verified) (error "Formatting changed Lisp forms")))
    (check-parens)
    (write-region (point-min) (point-max) "lisp/agent-shell-sidebar.el" nil 'silent)))
