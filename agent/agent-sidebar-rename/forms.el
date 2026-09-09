;;; forms.el --- Record top-level source forms -*- lexical-binding: t; -*-
(require 'json)
(let (files)
  (dolist (name '("agent-shell-sidebar" "agent-sidebar"))
    (with-temp-buffer
      (insert-file-contents (concat "/Users/ducnguyen/.emacs.d/agent/agent-sidebar-rename/before-" name ".el"))
      (emacs-lisp-mode)
      (goto-char (point-min))
      (let (forms)
        (while (progn (forward-comment (point-max)) (not (eobp)))
          (let* ((start (point)) (form (read (current-buffer))))
            (push (list :kind (symbol-name (car form))
                        :name (format "%s" (cadr form))
                        :text (buffer-substring-no-properties start (point)))
                  forms)))
        (push (cons name (vconcat (nreverse forms))) files))))
  (with-temp-file "/Users/ducnguyen/.emacs.d/agent/agent-sidebar-rename/forms.json"
    (insert (json-encode files))))
