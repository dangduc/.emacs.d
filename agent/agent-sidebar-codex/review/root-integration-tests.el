;;; root-integration-tests.el --- New-session command evidence -*- lexical-binding: t; -*-
(load (expand-file-name "../tests.el" (file-name-directory load-file-name)) nil t)

(ert-deftest review-root-new-session-pending-row-uses-recorded-directory ()
  (codex-test--fixture
    (let* ((actual (expand-file-name "different recorded repo/" temporary))
           (file (progn (make-directory actual t)
                        (codex-test--write agent-sidebar-codex-home actual)))
           (agent-shell-sidebar--context-roots (list root)) launched)
      (agent-sidebar-refresh)
      (merge--goto file)
      (should agent-sidebar--parse-queue)
      (cl-letf (((symbol-function 'agent-sidebar--require-codex-terminal) #'ignore)
                ((symbol-function 'agent-sidebar--launch-terminal)
                 (lambda (_name _program _args directory) (setq launched directory))))
        (agent-sidebar-new-session))
      (message "NEW PENDING ROW: expected=%S actual=%S" actual launched)
      (should (equal launched actual)))))
