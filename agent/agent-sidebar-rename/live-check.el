;;; live-check.el --- Load and verify Codex sidebar in running Emacs -*- lexical-binding: t; -*-
(require 'cl-lib)
(load "/Users/ducnguyen/.emacs.d/lisp/agent-sidebar.elc" nil t)
(require 'ghostel)
(cl-pushnew 'codex-cli agent-sidebar-enabled-providers :test #'eq)
(define-key agent-sidebar-mode-map (kbd "A") #'agent-sidebar-visit-in-agent-shell)

;; The menu has one new entry.  Evaluate its complete existing form only.
(with-temp-buffer
  (insert-file-contents "/Users/ducnguyen/.emacs.d/lisp/package-declarations.el")
  (search-forward "(defhydra hydra-submenu-agent-shell ")
  (goto-char (match-beginning 0))
  (eval (read (current-buffer)) t))

;; Refresh existing provider buffers with their marks and view state intact.
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'agent-sidebar-mode)
      (clrhash agent-sidebar--parse-cache)
      (agent-sidebar-refresh))))

(let* ((temporary (file-name-as-directory (make-temp-file "codex-sidebar-review-live-" t)))
       (root (expand-file-name "repo with space/" temporary))
       (home (expand-file-name "codex/" temporary))
       (file (expand-file-name "sessions/2026/09/08/rollout-fixture.jsonl" home))
       (output (expand-file-name "ghostel-args.json" temporary))
       (id "12345678-1234-7123-8123-123456789abc")
       (buffer (generate-new-buffer " *renamed sidebar live fixture*"))
       (started (float-time)) poller terminal)
  (make-directory root t)
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert (json-encode `((type . "session_meta")
                          (payload . ((id . ,id) (session_id . "not-the-thread-id")
                                      (cwd . ,root) (timestamp . "2026-09-08T19:00:00Z"))))) "\n"
            (json-encode '((type . "turn_context") (payload . ((model . "live-codex-model"))))) "\n"
            (json-encode '((type . "event_msg") (payload . ((type . "user_message") (message . "Live native Codex prompt"))))) "\n"))
  (with-current-buffer buffer
    (let ((agent-sidebar-refresh-timer nil)) (agent-sidebar-mode))
    (setq-local agent-sidebar-refresh-timer nil)
    (setq-local agent-sidebar-codex-home home)
    (setq-local agent-sidebar-enabled-providers '(codex-cli))
    (setq-local agent-sidebar-parse-idle-delay 0.05)
    (setq-local agent-sidebar-terminal-function nil)
    (setq-local agent-sidebar-codex-cli-command
                "/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/ghostel-cli-fixture.py")
    (agent-sidebar-refresh))
  (setq poller
        (run-at-time
         0.1 0.1
         (lambda ()
           (condition-case err
               (when (and (buffer-live-p buffer)
                          (null (buffer-local-value 'agent-sidebar--parse-timer buffer)))
                 (unless terminal
                   (with-current-buffer buffer
                     (let ((process-environment (cons (concat "SIDEBAR_CODEX_PROBE_OUTPUT=" output) process-environment)))
                       (save-window-excursion (agent-sidebar--codex-visit (gethash file agent-sidebar--entries)))))
                   (setq terminal (seq-find
                                   (lambda (candidate)
                                     (equal file (buffer-local-value 'agent-sidebar--terminal-file candidate)))
                                   (buffer-list))))
                 (when (file-exists-p output)
                   (let* ((data (with-temp-buffer (insert-file-contents output)
                                                 (json-parse-buffer :object-type 'alist :array-type 'list)))
                          (process (get-buffer-process terminal))
                          (result
                           (with-current-buffer buffer
                             (save-window-excursion (agent-sidebar--codex-visit (gethash file agent-sidebar--entries)))
                             (list :pid (emacs-pid) :depth (recursion-depth)
                                   :library (symbol-file 'agent-sidebar--codex-visit 'defun)
                                   :compiled (byte-code-function-p (symbol-function 'agent-sidebar--codex-visit))
                                   :parsed (hash-table-count agent-sidebar--parse-cache)
                                   :prompt-rendered (not (null (string-search "Live native Codex prompt" (buffer-string))))
                                   :model-rendered (not (null (string-search "live-codex-model (1)" (buffer-string))))
                                   :ghostel-mode (buffer-local-value 'major-mode terminal)
                                   :process-live (process-live-p process)
                                   :reused (eq process (get-buffer-process terminal))
                                   :argv-correct (equal (alist-get 'argv data) (list "resume" id))
                                   :cwd-correct (equal (file-truename (alist-get 'cwd data)) (directory-file-name (file-truename root)))
                                   :home-correct (equal (alist-get 'codex_home data) home)
                                   :agent-shell-command (commandp 'agent-sidebar-visit-in-agent-shell)
                                   :menu (lookup-key hydra-submenu-agent-shell/keymap (kbd "x"))
                                   :elapsed (- (float-time) started)))))
                     (with-temp-file "/Users/ducnguyen/.emacs.d/agent/agent-sidebar-rename/live-check.eldata"
                       (prin1 result (current-buffer)) (insert "\n")))
                   (cancel-timer poller)
                   (when (buffer-live-p terminal)
                     (with-current-buffer terminal (setq-local kill-buffer-query-functions nil) (kill-buffer terminal)))
                   (kill-buffer buffer)
                   (delete-directory temporary t)))
             (error
              (with-temp-file "/Users/ducnguyen/.emacs.d/agent/agent-sidebar-rename/live-check.eldata"
                (prin1 (list :error (error-message-string err)) (current-buffer)))
              (cancel-timer poller)
              (when (buffer-live-p terminal)
                (with-current-buffer terminal (setq-local kill-buffer-query-functions nil) (kill-buffer terminal)))
              (when (buffer-live-p buffer) (kill-buffer buffer))
              (delete-directory temporary t)))
           (when (and (buffer-live-p buffer) (> (- (float-time) started) 10))
             (cancel-timer poller)
             (with-temp-file "/Users/ducnguyen/.emacs.d/agent/agent-sidebar-rename/live-check.eldata"
               (prin1 '(:error "Live check timed out") (current-buffer)))
             (when (buffer-live-p terminal)
               (with-current-buffer terminal (setq-local kill-buffer-query-functions nil) (kill-buffer terminal)))
             (kill-buffer buffer)
             (delete-directory temporary t))))))

(provide 'agent-sidebar-codex-live-check)
