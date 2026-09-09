;;; kingsbury-tests.el --- State-transition probes -*- lexical-binding: t; -*-
(load (expand-file-name "../../tests.el" (file-name-directory load-file-name)) nil t)
(defconst kingsbury--fixture
  (expand-file-name "kingsbury-acp-fixture.py" (file-name-directory load-file-name)))

(ert-deftest kingsbury-new-acp-session-protects-its-later-rollout ()
  "C-u N -> session/new -> rollout discovery -> deletion must retain the live file."
  (codex-test--fixture
    (let* ((seed (codex-test--write agent-sidebar-codex-home root))
           (log (expand-file-name "acp.jsonl" temporary))
           (agent-shell-openai-codex-acp-command
            (list (executable-find "python3") kingsbury--fixture log))
           (config (agent-shell-openai-make-codex-config))
           (agent-shell-agent-configs (list config))
           (agent-shell-openai-authentication (agent-shell-openai-make-authentication :login t))
           (agent-shell-prefer-viewport-interaction nil)
           (agent-shell-show-welcome-message nil) (agent-shell-show-config-icons nil)
           (agent-shell-header-style 'text) (agent-shell-inhibit-system-sleep nil)
           (agent-shell-session-restore-verbosity 'full)
           (enable-local-variables nil) (enable-local-eval nil)
           (delete-by-moving-to-trash nil) shown live-file)
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell-sidebar--pop-to) (lambda (target) (setq shown target))))
            (agent-sidebar-refresh) (merge--drain) (merge--goto seed)
            (agent-sidebar-new-session t)
            (let ((deadline (+ (float-time) 8)))
              (while (and (< (float-time) deadline)
                          (not (equal codex-test--other-id
                                      (map-nested-elt (buffer-local-value 'agent-shell--state shown) '(:session :id)))))
                (accept-process-output nil 0.02)))
            (should (buffer-live-p shown))
            (should (equal codex-test--other-id
                           (map-nested-elt (buffer-local-value 'agent-shell--state shown) '(:session :id))))
            ;; A real adapter writes this native file once session/new succeeds.
            (setq live-file (codex-test--write agent-sidebar-codex-home root codex-test--other-id))
            (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list shown))))
              (agent-sidebar-refresh) (merge--drain) (merge--goto live-file)
              (should (equal (plist-get (agent-sidebar--cached-meta (gethash live-file agent-sidebar--entries)) :session-id)
                             codex-test--other-id))
              (message "NEW ACP ownership: active-id=%S native-marker=%S owned=%S"
                       (map-nested-elt (buffer-local-value 'agent-shell--state shown) '(:session :id))
                       (buffer-local-value 'agent-sidebar--agent-shell-file shown)
                       (agent-sidebar--owned-file-p live-file))
              (agent-sidebar-mark-delete)
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                (agent-sidebar-execute))
              (message "NEW ACP after delete: exists=%S retained-mark=%S active-id=%S"
                       (file-exists-p live-file) (gethash live-file agent-sidebar--marks)
                       (map-nested-elt (buffer-local-value 'agent-shell--state shown) '(:session :id)))
              (should (file-exists-p live-file))
              (should (eq (gethash live-file agent-sidebar--marks) 'delete))))
        (when (buffer-live-p shown)
          (with-current-buffer shown (setq-local kill-buffer-query-functions nil) (kill-buffer shown)))))))

(ert-deftest kingsbury-activation-aborts-after-reentrant-removal ()
  "A row removed during its metadata read cannot launch from that obsolete read."
  (codex-test--fixture
    (let* ((file (codex-test--write agent-sidebar-codex-home root))
           (original (symbol-function 'agent-sidebar--codex-parse))
           (sidebar (current-buffer)) calls removed)
      (agent-sidebar-refresh)
      ;; Activate before idle parsing so the read is performed by the action.
      (merge--goto file)
      (cl-letf (((symbol-function 'agent-sidebar--require-codex-terminal) #'ignore)
                ((symbol-function 'agent-sidebar--launch-terminal)
                 (lambda (&rest args) (push args calls) sidebar))
                ((symbol-function 'agent-sidebar--codex-parse)
                 (lambda (entry)
                   (prog1 (funcall original entry)
                     (unless removed
                       (setq removed t)
                       (delete-file file)
                       (with-current-buffer sidebar (agent-sidebar-refresh)))))))
        (condition-case nil (agent-sidebar-visit) (user-error nil)))
      (should removed)
      (should-not (gethash file agent-sidebar--entries))
      (message "REENTRANT action: removed=%S current-entry=%S launches=%S"
               removed (gethash file agent-sidebar--entries) (length calls))
      (should-not calls))))

(ert-deftest kingsbury-existing-resume-path-owner-control ()
  "The ownership guard works for its supported path-based happy path."
  (codex-test--fixture
    (let ((file (codex-test--write agent-sidebar-codex-home root))
          (owner (generate-new-buffer " *kingsbury owner*")))
      (unwind-protect
          (progn
            (with-current-buffer owner
              (setq major-mode 'agent-shell-mode)
              (setq-local agent-sidebar--agent-shell-file file))
            (should (agent-sidebar--owned-file-p file)))
        (kill-buffer owner)))))
