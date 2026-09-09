;;; inventory.el --- Read forms without loading either library -*- lexical-binding: t; -*-
(require 'json)
(let (result)
  (dolist (path command-line-args-left)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (let (definitions form)
        (condition-case nil
            (while t
              (forward-comment (point-max))
              (let ((line (line-number-at-pos)))
                (setq form (read (current-buffer)))
                (when (and (consp form)
                           (memq (car form) '(defun cl-defun iter-defun defmacro
                                             defcustom defvar defvar-local defconst
                                             defface defgroup define-derived-mode))
                           (symbolp (cadr form)))
                  (let* ((name (symbol-name (cadr form)))
                         (normalized (replace-regexp-in-string "agent-sidebar" "vegeta" name)))
                    (when (string-prefix-p "vegeta" normalized)
                      (push `((name . ,normalized) (kind . ,(symbol-name (car form)))
                              (line . ,line)
                              (form . ,(replace-regexp-in-string
                                        "agent-sidebar" "vegeta" (prin1-to-string form))))
                            definitions))))))
          (end-of-file nil))
        (push (cons path (vconcat (nreverse definitions))) result))))
  (princ (json-encode (nreverse result))))
