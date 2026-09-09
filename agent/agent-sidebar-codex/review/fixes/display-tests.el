;;; display-tests.el --- Display transaction regressions -*- lexical-binding: t; -*-
(load (expand-file-name "../round-2/torvalds-tests.el" (file-name-directory load-file-name)) nil t)
(load (expand-file-name "terminal-tests.el" (file-name-directory load-file-name)) nil t)
(define-error 'display-fix-error "Injected display failure")

(defun display-fix--invoke (function)
  "Return FUNCTION's error or quit unchanged for assertion."
  (condition-case actual (funcall function)
    ((error quit) actual)))

(ert-deftest display-fix-new-custom-buffer-error-and-quit-clean-up ()
  (torvalds-r2--fixture
    (dolist (condition '(display-fix-error quit))
      (let (target child queried
            (expected (list condition "original display condition")))
        (let ((agent-sidebar-terminal-function
               (lambda (&rest _)
                 (setq target (generate-new-buffer " *display new custom*"))
                 (setq child (make-process :name "display-custom" :buffer target
                                           :command '("/bin/cat") :noquery nil))
                 (push child processes)
                 (with-current-buffer target
                   (setq-local kill-buffer-query-functions
                               (list (lambda () (setq queried t) (error "Unexpected kill query")))))
                 target)))
          (cl-letf (((symbol-function 'agent-shell-sidebar--pop-to)
                     (lambda (_) (signal condition (cdr expected)))))
            (should (equal expected
                           (display-fix--invoke
                            (lambda () (agent-sidebar--launch-terminal "display" "/bin/cat" nil directory)))))))
        (should-not queried)
        (should-not (buffer-live-p target))
        (should-not (process-live-p child))))))

(ert-deftest display-fix-existing-custom-buffer-error-and-quit-preserve-owner ()
  (torvalds-r2--fixture
    (let* ((target (generate-new-buffer " *display existing custom*"))
           (child (make-process :name "display-existing" :buffer target
                                :command '("/bin/cat") :noquery t))
           (agent-sidebar-terminal-function (lambda (&rest _) target))
           queried)
      (push child processes)
      (with-current-buffer target
        (insert "original contents")
        (setq-local kill-buffer-query-functions
                    (list (lambda () (setq queried t) (error "Unexpected owner query")))))
      (dolist (condition '(display-fix-error quit))
        (let ((expected (list condition "original display condition")))
          (cl-letf (((symbol-function 'agent-shell-sidebar--pop-to)
                     (lambda (_) (signal condition (cdr expected)))))
            (should (equal expected
                           (display-fix--invoke
                            (lambda () (agent-sidebar--launch-terminal "display" "/bin/cat" nil directory)))))))
        (should-not queried)
        (should (buffer-live-p target))
        (should (process-live-p child))
        (should (equal "original contents" (with-current-buffer target (buffer-string))))))))

(ert-deftest display-fix-ghostel-dispatch-error-and-quit-clean-up ()
  (torvalds-r2--fixture
    (dolist (condition '(display-fix-error quit))
      (let ((agent-sidebar-terminal-function nil) target child
            (expected (list condition "original display condition")))
        (cl-letf (((symbol-function 'ghostel-exec)
                   (lambda (buffer program args)
                     (should (equal program "/bin/cat")) (should-not args)
                     (setq target buffer child (make-process :name "display-ghostel" :buffer buffer
                                                            :command '("/bin/cat") :noquery nil))
                     (push child processes) child))
                  ((symbol-function 'agent-shell-sidebar--pop-to)
                   (lambda (_) (signal condition (cdr expected)))))
          (should (equal expected
                         (display-fix--invoke
                          (lambda () (agent-sidebar--launch-terminal "display" "/bin/cat" nil directory))))))
        (should-not (buffer-live-p target))
        (should-not (process-live-p child))))))

(ert-deftest display-fix-existing-session-reuse-preserves-running-child ()
  (torvalds-r2--fixture
    (let* ((target (generate-new-buffer " *display existing resume*"))
           (child (make-process :name "display-resume" :buffer target
                                :command '("/bin/cat") :noquery t))
           (session "12345678-1111-7111-8111-111111111111")
           (entry (list :provider 'claude-cli :id (expand-file-name "rollout.jsonl" temporary))))
      (push child processes)
      (with-current-buffer target
        (setq-local agent-sidebar--terminal-session (list 'claude-cli session directory)))
      (cl-letf (((symbol-function 'agent-sidebar--ensure-parsed)
                 (lambda (_) (list :session-id session :cwd directory)))
                ((symbol-function 'agent-sidebar--launch-terminal)
                 (lambda (&rest _) (ert-fail "Attempted duplicate launch")))
                ((symbol-function 'agent-shell-sidebar--pop-to)
                 (lambda (_) (signal 'display-fix-error '("existing display failure")))))
        (should (equal '(display-fix-error "existing display failure")
                       (display-fix--invoke
                        (lambda () (agent-sidebar--visit-terminal entry "review" "/bin/cat" nil))))))
      (should (buffer-live-p target))
      (should (process-live-p child)))))
