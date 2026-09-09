;;; group-membership-tests.el --- Group membership and bounded rejection -*- lexical-binding: t; -*-
(load (expand-file-name "../round-2/ousterhout-tests.el" (file-name-directory load-file-name)) nil t)

(ert-deftest fix-group-changed-filter-membership-aborts ()
  (codex-test--fixture
    (let* ((file (codex-test--write agent-sidebar-codex-home root)) launches)
      (agent-sidebar-refresh) (merge--drain)
      (agent-sidebar-set-grouping '(repo))
      (agent-sidebar-set-filter "Native Codex prompt")
      (goto-char (point-min))
      (with-temp-file file
        (insert (json-encode `((type . "session_meta") (payload . ((id . ,codex-test--id) (cwd . ,root))))) "\n"
                (json-encode '((type . "event_msg") (payload . ((type . "user_message") (message . "Different visible content"))))) "\n"))
      (cl-letf (((symbol-function 'agent-sidebar--require-codex-terminal) #'ignore)
                ((symbol-function 'agent-sidebar--launch-terminal)
                 (lambda (&rest args) (push args launches))))
        (should-error (agent-sidebar-new-session) :type 'user-error))
      (should-not launches))))

(ert-deftest fix-group-changed-model-membership-aborts ()
  (codex-test--fixture
    (let ((file (codex-test--write agent-sidebar-codex-home root)) launches)
      (agent-sidebar-refresh) (merge--drain)
      (agent-sidebar-set-grouping '(model repo))
      (goto-char (point-min))
      (with-temp-file file
        (insert (json-encode `((type . "session_meta") (payload . ((id . ,codex-test--id) (cwd . ,root))))) "\n"
                (json-encode '((type . "turn_context") (payload . ((model . "different-model"))))) "\n"))
      (cl-letf (((symbol-function 'agent-sidebar--require-codex-terminal) #'ignore)
                ((symbol-function 'agent-sidebar--launch-terminal)
                 (lambda (&rest args) (push args launches))))
        (should-error (agent-sidebar-new-session) :type 'user-error))
      (should-not launches))))

(ert-deftest fix-group-stops-at-second-distinct-context ()
  (codex-test--fixture
    (let ((other (expand-file-name "another-directory/" temporary))
          (original (symbol-function 'agent-sidebar--codex-parse))
          (calls 0))
      (make-directory other t)
      (dotimes (n 20)
        (let* ((id (format "12345678-1234-7123-8123-%012d" n))
               (file (codex-test--write agent-sidebar-codex-home (if (= (% n 2) 0) root other) id)))
          (set-file-times file (seconds-to-time (+ 1800000000 n)))))
      (agent-sidebar-refresh)
      (should (= (length agent-sidebar--parse-queue) 20))
      (cl-letf (((symbol-function 'agent-sidebar--codex-parse)
                 (lambda (entry)
                   (cl-incf calls)
                   (when (> calls 2) (ert-fail "Parsed beyond established group ambiguity"))
                   (funcall original entry))))
        (should-error (agent-sidebar--new-context-for-group '((package . codex-cli))) :type 'user-error))
      (should (= calls 2))
      (message "EARLY AMBIGUITY: 20 discovered entries, %d parser calls" calls))))

(ert-deftest fix-group-warm-one-context-keeps-cache-and-discovery ()
  (codex-test--fixture
    (dotimes (n 20)
      (codex-test--write agent-sidebar-codex-home root (format "12345678-1234-7123-8123-%012d" n)))
    (agent-sidebar-refresh) (merge--drain)
    (cl-letf (((symbol-function 'agent-sidebar--codex-parse)
               (lambda (&rest _) (ert-fail "Warm group reparsed unchanged metadata")))
              ((symbol-function 'agent-sidebar--codex-list)
               (lambda (&rest _) (ert-fail "Group rediscovered files"))))
      (let ((context (agent-sidebar--new-context-for-group '((package . codex-cli)))))
        (should (equal (plist-get context :directory) root))
        (should (equal (plist-get context :codex-home) agent-sidebar-codex-home))
        (should (eq (plist-get context :provider) 'codex-cli))))))
