;;; ownership-tests.el --- Native session ownership regressions -*- lexical-binding: t; -*-
(load (expand-file-name "../round-1/kingsbury-tests.el" (file-name-directory load-file-name)) nil t)
(defmacro ownership--acp-fixture (&rest body)
  (declare (indent 0))
  `(codex-test--fixture
     (let* ((log (expand-file-name "acp.jsonl" temporary))
            (agent-shell-openai-codex-acp-command
             (list (executable-find "python3") codex-test--fixture log))
            (agent-shell-agent-configs (list (agent-shell-openai-make-codex-config)))
            (agent-shell-openai-authentication (agent-shell-openai-make-authentication :login t))
            (agent-shell-prefer-viewport-interaction nil)
            (agent-shell-show-welcome-message nil) (agent-shell-show-config-icons nil)
            (agent-shell-header-style 'text) (agent-shell-inhibit-system-sleep nil)
            (agent-shell-session-restore-verbosity 'full)
            (enable-local-variables nil) (enable-local-eval nil)
            (original-start (symbol-function 'agent-shell--start)) shells)
       (unwind-protect
           (cl-letf (((symbol-function 'agent-shell--start)
                      (lambda (&rest args)
                        (let ((shell (apply original-start args)))
                          (push shell shells) shell)))
                     ((symbol-function 'agent-shell--display-buffer) #'ignore))
             ,@body)
         (dolist (shell shells)
           (when (buffer-live-p shell)
             (with-current-buffer shell
               (setq-local kill-buffer-query-functions nil)
               (kill-buffer shell))))))))

(defun ownership--wait-session (shell)
  (let ((deadline (+ (float-time) 8)))
    (while (and (< (float-time) deadline)
                (not (equal codex-test--id
                            (map-nested-elt (buffer-local-value 'agent-shell--state shell) '(:session :id)))))
      (accept-process-output nil 0.02)))
  (should (equal codex-test--id
                 (map-nested-elt (buffer-local-value 'agent-shell--state shell) '(:session :id)))))

(defmacro ownership--state-fixture (&rest body)
  (declare (indent 0))
  `(codex-test--fixture
     (let* ((file (codex-test--write agent-sidebar-codex-home root))
            (owner (generate-new-buffer " *ownership state fixture*"))
            (config (agent-shell-openai-make-codex-config)))
       (unwind-protect
           (progn
             (with-current-buffer owner
               (setq major-mode 'agent-shell-mode)
               (setq-local agent-shell--state
                           (list (cons :agent-config config)
                                 (cons :session (list (cons :id codex-test--id)))
                                 (cons :client
                                       (list (cons :environment-variables
                                                   (list (concat "CODEX_HOME=" agent-sidebar-codex-home))))))))
             ,@body)
         (when (buffer-live-p owner) (kill-buffer owner))))))

(ert-deftest ownership-new-session-real-acp ()
  (funcall (ert-test-body (ert-get-test 'kingsbury-new-acp-session-protects-its-later-rollout))))

(ert-deftest ownership-reload-uses-session-without-native-path-marker ()
  (ownership--acp-fixture
    (let* ((file (codex-test--write agent-sidebar-codex-home root))
           (shell (agent-sidebar--codex-start-agent-shell root codex-test--id)))
      (ownership--wait-session shell)
      (should (agent-sidebar--owned-file-p file))
      (with-current-buffer shell (agent-shell-reload))
      (should-not (buffer-live-p shell))
      (ownership--wait-session (car shells))
      (should-not (buffer-local-value 'agent-sidebar--agent-shell-file (car shells)))
      (should-not (buffer-local-value 'agent-sidebar--codex-session-home (car shells)))
      (should (agent-sidebar--owned-file-p file))
      (message "Ownership survives actual ACP reload with both legacy markers absent"))))

(ert-deftest ownership-home-config-and-session-controls ()
  (ownership--state-fixture
    (should (agent-sidebar--owned-file-p file))
    (let ((other-file (codex-test--write (expand-file-name "other-codex/" temporary) root)))
      (should-not (agent-sidebar--owned-file-p other-file)))
    (let ((other-session (codex-test--write agent-sidebar-codex-home root codex-test--other-id)))
      (should-not (agent-sidebar--owned-file-p other-session)))
    (with-current-buffer owner
      (setf (alist-get :agent-config agent-shell--state) '((:identifier . claude))))
    (should-not (agent-sidebar--owned-file-p file))
    (with-current-buffer owner
      (setf (alist-get :agent-config agent-shell--state) config
            (alist-get :client agent-shell--state) nil))
    (should-not (agent-sidebar--owned-file-p file))))

(ert-deftest ownership-active-id-precedes-pending-and-transitions ()
  (ownership--state-fixture
    (with-current-buffer owner
      (setf (alist-get :resume-session-id agent-shell--state) codex-test--id
            (alist-get :session agent-shell--state) (list (cons :id codex-test--other-id))))
    (should-not (agent-sidebar--owned-file-p file))
    (with-current-buffer owner (setf (alist-get :session agent-shell--state) nil))
    (should (agent-sidebar--owned-file-p file))
    (with-current-buffer owner (setf (alist-get :resume-session-id agent-shell--state) nil))
    (should-not (agent-sidebar--owned-file-p file))))

(ert-deftest ownership-check-reads-only-selected-file-and-refreshes-metadata ()
  (ownership--state-fixture
    (codex-test--write agent-sidebar-codex-home root codex-test--other-id)
    (let ((original (symbol-function 'agent-sidebar--read-jsonl)) reads)
      (cl-letf (((symbol-function 'agent-sidebar--read-jsonl)
                 (lambda (selected limit)
                   (push (cons selected limit) reads)
                   (funcall original selected limit))))
        (should (agent-sidebar--owned-file-p file)))
      (should (equal reads (list (cons file 262144)))))
    (agent-sidebar-refresh) (merge--drain)
    ;; Same path and previously cached metadata now describe another session.
    (with-temp-file file
      (insert (json-encode `((type . "session_meta")
                            (payload . ((id . ,codex-test--other-id) (cwd . ,root))))) "\n"))
    (should-not (agent-sidebar--owned-file-p file))))

(ert-deftest ownership-rechecks-live-state-after-reading ()
  (ownership--state-fixture
    (let ((original (symbol-function 'agent-sidebar--codex-parse)))
      (cl-letf (((symbol-function 'agent-sidebar--codex-parse)
                 (lambda (entry)
                   (prog1 (funcall original entry)
                     (with-current-buffer owner
                       (setf (alist-get :session agent-shell--state)
                             (list (cons :id codex-test--other-id))))))))
        (should-not (agent-sidebar--owned-file-p file))))))

(ert-deftest ownership-malformed-prefix-retains-deletion-mark ()
  (ownership--state-fixture
    (let ((delete-by-moving-to-trash nil))
      (with-temp-file file (insert "malformed\n"))
      (agent-sidebar-refresh)
      (puthash file 'delete agent-sidebar--marks)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (agent-sidebar-execute))
      (should (file-exists-p file))
      (should (eq (gethash file agent-sidebar--marks) 'delete)))))

(ert-deftest ownership-existing-marker-and-file-alias-control ()
  (ownership--state-fixture
    (let ((alias (expand-file-name "alias.jsonl" temporary)))
      (make-symbolic-link file alias)
      (with-current-buffer owner (setq-local agent-sidebar--agent-shell-file file))
      (should (agent-sidebar--owned-file-p alias)))))

(ert-deftest ownership-absent-file-does-not-retain-stale-mark ()
  (ownership--state-fixture
    (agent-sidebar-refresh)
    (delete-file file)
    (puthash file 'delete agent-sidebar--marks)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (agent-sidebar-execute))
    (should-not (gethash file agent-sidebar--marks))))
