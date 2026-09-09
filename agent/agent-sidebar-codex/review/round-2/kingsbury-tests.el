;;; kingsbury-tests.el --- Round 2 lifecycle probes -*- lexical-binding: t; -*-
(load (expand-file-name "../fixes/ownership-tests.el" (file-name-directory load-file-name)) nil t)

(ert-deftest kingsbury-r2-reload-during-file-read-retains-active-rollout ()
  "The owner may be replaced by actual agent-shell reload during a file hook."
  (ownership--acp-fixture
    (let* ((file (codex-test--write agent-sidebar-codex-home root))
           (shell (agent-sidebar--codex-start-agent-shell root codex-test--id))
           (delete-by-moving-to-trash nil) reloaded)
      (ownership--wait-session shell)
      (agent-sidebar-refresh) (merge--drain)
      (puthash file 'delete agent-sidebar--marks)
      (let ((after-insert-file-functions
             (list
              (lambda (_length)
                (when (and (not reloaded)
                           (string-search "session_meta" (buffer-string)))
                  (setq reloaded t)
                  (with-current-buffer shell (agent-shell-reload))
                  (ownership--wait-session (car shells)))
                nil))))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (agent-sidebar-execute)))
      (should reloaded)
      (should-not (buffer-live-p shell))
      (should (buffer-live-p (car shells)))
      (should (equal codex-test--id
                     (map-nested-elt (buffer-local-value 'agent-shell--state (car shells)) '(:session :id))))
      (message "RELOAD DURING READ: old-live=%S new-live=%S file-exists=%S retained-mark=%S"
               (buffer-live-p shell) (buffer-live-p (car shells))
               (file-exists-p file) (gethash file agent-sidebar--marks))
      (should (file-exists-p file))
      (should (eq (gethash file agent-sidebar--marks) 'delete)))))

(ert-deftest kingsbury-r2-state-switch-to-other-home-is-not-an-owner ()
  "A session in another home must not protect this home's matching UUID."
  (ownership--state-fixture
    (let* ((original (symbol-function 'agent-sidebar--codex-parse))
           (other-home (expand-file-name "other-home/" temporary)))
      (make-directory other-home)
      (cl-letf (((symbol-function 'agent-sidebar--codex-parse)
                 (lambda (entry)
                   (prog1 (funcall original entry)
                     (with-current-buffer owner
                       (setf (alist-get :client agent-shell--state)
                             (list (cons :environment-variables
                                         (list (concat "CODEX_HOME=" other-home))))))))))
        (should-not (agent-sidebar--owned-file-p file))))))

(ert-deftest kingsbury-r2-mutation-disabled-native-guard-exposes-deletion ()
  "Removing the new native guard must expose actual deletion, not another guard."
  (ownership--state-fixture
    (let ((delete-by-moving-to-trash nil))
      (agent-sidebar-refresh) (merge--drain)
      (should (agent-sidebar--owned-file-p file))
      (puthash file 'delete agent-sidebar--marks)
      (cl-letf (((symbol-function 'agent-sidebar--codex-owned-file-p) (lambda (_) nil))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (agent-sidebar-execute))
      (should-not (file-exists-p file))
      (should-not (gethash file agent-sidebar--marks))
      (message "MUTATION CONTROL: disabling native guard deletes the active fixture file"))))
