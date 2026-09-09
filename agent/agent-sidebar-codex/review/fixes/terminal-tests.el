;;; terminal-tests.el --- Terminal fix regression checks -*- lexical-binding: t; -*-
(load (expand-file-name "../round-1/torvalds-tests.el" (file-name-directory load-file-name)) nil t)
(load (expand-file-name "../../tests.el" (file-name-directory load-file-name)) nil t)

;; The row action now checks current membership and reparses its metadata.
;; Preserve the original public-command expectation with a valid row fixture.
(ert-delete-test 'torvalds-term-new-session-really-starts-new-process)
(ert-deftest torvalds-term-new-session-really-starts-new-process ()
  (torvalds--term-fixture
    (let ((agent-sidebar-claude-cli-command "/bin/cat") shown first-buffer)
      (with-temp-buffer
        (agent-sidebar-mode)
        (let ((entry (list :provider 'claude-cli :repo directory
                           :id (expand-file-name "fixture.jsonl" temporary)))
              (inhibit-read-only t))
          (puthash (plist-get entry :id) entry agent-sidebar--entries)
          (insert (propertize "fixture row\n" 'agent-sidebar-entry entry)))
        (goto-char (point-min))
        (cl-letf (((symbol-function 'ghostel-exec) nil)
                  ((symbol-function 'vterm) nil)
                  ((symbol-function 'agent-sidebar--ensure-parsed)
                   (lambda (_) (list :cwd directory :agent "Claude")))
                  ((symbol-function 'agent-shell-sidebar--pop-to) (lambda (buffer) (setq shown buffer))))
          (agent-sidebar-new-session)
          (setq first-buffer shown)
          (push (get-buffer-process shown) processes)
          (set-process-query-on-exit-flag (car processes) nil)
          (should (process-live-p (car processes)))
          (agent-sidebar-new-session)
          (push (get-buffer-process shown) processes)
          (message "TERM-NEW: same-buffer=%S, same-process=%S"
                   (eq first-buffer shown) (eq (car processes) (cadr processes)))
          (should-not (eq first-buffer shown))
          (should-not (eq (car processes) (cadr processes))))))))

(ert-deftest terminal-fix-cleanup-before-child-start-preserves-existing-buffer ()
  (torvalds--term-fixture
    (let ((existing (get-buffer-create "*terminal-fix*")) failed)
      (with-current-buffer existing (insert "existing contents"))
      (cl-letf (((symbol-function 'ghostel-exec) nil)
                ((symbol-function 'vterm) nil)
                ((symbol-function 'make-term)
                 (lambda (name &rest _)
                   (setq failed (get-buffer (concat "*" name "*")))
                   (should (buffer-live-p failed))
                   (should-not (eq existing failed))
                   (error "Fixture fails before creating a process"))))
        (should-error (agent-sidebar--launch-terminal "terminal-fix" "/bin/cat" nil directory))
        (should-not (buffer-live-p failed))
        (should (buffer-live-p existing))
        (should (equal "existing contents" (with-current-buffer existing (buffer-string))))))))

(ert-deftest terminal-fix-cleanup-after-child-start-does-not-query ()
  (torvalds--term-fixture
    (let (failed child)
      (cl-letf (((symbol-function 'ghostel-exec) nil)
                ((symbol-function 'vterm) nil)
                ((symbol-function 'term-char-mode)
                 (lambda ()
                   (setq failed (current-buffer) child (get-buffer-process failed))
                   (push child processes)
                   (should (process-live-p child))
                   (setq-local kill-buffer-query-functions
                               (list (lambda () (ert-fail "Cleanup requested confirmation"))))
                   (error "Fixture fails after creating a process"))))
        (should-error (agent-sidebar--launch-terminal "terminal-fix" "/bin/cat" nil directory))
        (should-not (buffer-live-p failed))
        (should-not (process-live-p child))))))

(ert-deftest terminal-fix-cleanup-on-quit ()
  (torvalds--term-fixture
    (let (failed caught)
      (cl-letf (((symbol-function 'ghostel-exec) nil)
                ((symbol-function 'vterm) nil)
                ((symbol-function 'make-term)
                 (lambda (name &rest _)
                   (setq failed (get-buffer (concat "*" name "*")))
                   (signal 'quit nil))))
        (condition-case nil
            (agent-sidebar--launch-terminal "terminal-fix" "/bin/cat" nil directory)
          (quit (setq caught t)))
        (should caught)
        (should-not (buffer-live-p failed))))))
