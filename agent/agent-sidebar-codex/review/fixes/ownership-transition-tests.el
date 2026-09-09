;;; ownership-transition-tests.el --- Replacement-owner controls -*- lexical-binding: t; -*-
(load (expand-file-name "../round-2/kingsbury-tests.el" (file-name-directory load-file-name)) nil t)

(ert-deftest ownership-transition-replacement-matrix-reads-file-once ()
  "A replacement owner needs both the selected UUID and its verified home."
  (dolist (scenario '(same-session other-session other-home))
    (ownership--state-fixture
      (let ((original (symbol-function 'agent-sidebar--codex-parse)) replacement
            (reads 0) owned)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'agent-sidebar--codex-parse)
                         (lambda (entry)
                           (cl-incf reads)
                           (prog1 (funcall original entry)
                             (setq replacement (generate-new-buffer " *ownership replacement*"))
                             (let ((state (copy-tree (buffer-local-value 'agent-shell--state owner))))
                               (with-current-buffer replacement
                                 (setq major-mode 'agent-shell-mode)
                                 (setq-local agent-shell--state state)
                                 (pcase scenario
                                   ('other-session
                                    (setf (alist-get :session agent-shell--state)
                                          (list (cons :id codex-test--other-id))))
                                   ('other-home
                                    (setf (alist-get :client agent-shell--state)
                                          (list (cons :environment-variables
                                                      (list (concat "CODEX_HOME="
                                                                    (expand-file-name "other-home/" temporary))))))))))
                             (kill-buffer owner)))))
                (setq owned (agent-sidebar--owned-file-p file)))
              (should (= reads 1))
              (should (equal (not (null owned)) (eq scenario 'same-session)))
              (message "Replacement matrix: %S ownership=%S reads=%d" scenario (not (null owned)) reads))
          (when (buffer-live-p replacement) (kill-buffer replacement)))))))
