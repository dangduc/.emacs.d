;;; ousterhout-tests.el --- Configuration ownership evidence -*- lexical-binding: t; -*-
(load "/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/tests.el" nil t)

(defmacro ousterhout--acp-fixture (&rest body)
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

(defun ousterhout--wait-session (shell)
  (let ((deadline (+ (float-time) 8)))
    (while (and (< (float-time) deadline)
                (not (equal codex-test--id
                            (map-nested-elt (buffer-local-value 'agent-shell--state shell)
                                            '(:session :id)))))
      (accept-process-output nil 0.02)))
  (should (equal codex-test--id
                 (map-nested-elt (buffer-local-value 'agent-shell--state shell) '(:session :id)))))

(defun ousterhout--child-homes (log)
  (with-temp-buffer
    (insert-file-contents log)
    (delq nil
          (mapcar (lambda (line)
                    (map-nested-elt (json-parse-string line :object-type 'alist)
                                    '(fixture_environment codex_home)))
                  (split-string (buffer-string) "\n" t)))))

(ert-deftest ousterhout-selected-home-overrides-configured-acp-home ()
  "The selected transcript's store must reach the actual child process."
  (ousterhout--acp-fixture
    (let* ((other-home (expand-file-name "other-codex/" temporary))
           (agent-shell-openai-codex-environment (list (concat "CODEX_HOME=" other-home)))
           (shell (agent-sidebar--codex-start-agent-shell root codex-test--id)))
      (ousterhout--wait-session shell)
      (message "ACP client explicit CODEX_HOME entries=%S"
               (seq-filter (lambda (v) (string-prefix-p "CODEX_HOME=" v))
                           (map-nested-elt (buffer-local-value 'agent-shell--state shell)
                                           '(:client :environment-variables))))
      (message "Configured home probe: selected=%s child=%S" agent-sidebar-codex-home
               (ousterhout--child-homes log))
      (should (equal (ousterhout--child-homes log) (list agent-sidebar-codex-home))))))

(ert-deftest ousterhout-reload-preserves-selected-home ()
  "Reload after the sidebar call returns must retain the same Codex store."
  (ousterhout--acp-fixture
    (let* ((process-environment (cons (concat "CODEX_HOME=" (expand-file-name "ambient/" temporary))
                                      (seq-remove (lambda (v) (string-prefix-p "CODEX_HOME=" v))
                                                  process-environment)))
           (agent-shell-openai-codex-environment nil)
           (shell (agent-sidebar--codex-start-agent-shell root codex-test--id)))
      (ousterhout--wait-session shell)
      (should (equal (ousterhout--child-homes log) (list agent-sidebar-codex-home)))
      (with-current-buffer shell (agent-shell-reload))
      (ousterhout--wait-session (car shells))
      (message "Reload home probe: selected=%s children=%S" agent-sidebar-codex-home
               (ousterhout--child-homes log))
      (should (equal (ousterhout--child-homes log)
                     (list agent-sidebar-codex-home agent-sidebar-codex-home))))))

(ert-deftest ousterhout-reuse-searches-all-matching-home-candidates ()
  "A different home's matching thread must not hide the correct live shell."
  (codex-test--fixture
    (let* ((config (agent-shell-openai-make-codex-config))
           (agent-shell-agent-configs (list config))
           (wrong (generate-new-buffer " *wrong home*"))
           (right (generate-new-buffer " *right home*"))
           (started (generate-new-buffer " *duplicate home*"))
           (starts 0))
      (unwind-protect
          (progn
            (dolist (shell (list wrong right))
              (with-current-buffer shell
                (setq-local default-directory root)
                (setq-local agent-shell--state
                            `((:session . ((:id . ,codex-test--id))) (:agent-config . ,config)))))
            (with-current-buffer wrong
              (setq-local agent-sidebar--codex-session-home (expand-file-name "other/" temporary)))
            (with-current-buffer right
              (setq-local agent-sidebar--codex-session-home (file-truename agent-sidebar-codex-home)))
            (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list wrong right)))
                      ((symbol-function 'agent-shell-sidebar--start)
                       (lambda (&rest _) (cl-incf starts) started)))
              (let ((result (agent-sidebar--codex-start-agent-shell root codex-test--id)))
                (message "Multi-home reuse probe: returned=%s starts=%d" (buffer-name result) starts)
                (should (eq result right))
                (should (= starts 0)))))
        (dolist (shell (list wrong right started)) (when (buffer-live-p shell) (kill-buffer shell)))))))
