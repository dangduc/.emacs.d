;;; live-check.el --- Load the merge and verify real idle timers -*- lexical-binding: t; -*-
(require 'cl-lib)

;; Preserve existing sidebar views if this check is run again later.
(let (owners)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-sidebar-mode)
        (push (list buffer major-mode agent-shell-sidebar--marks
                    agent-shell-sidebar--collapsed agent-shell-sidebar--filter
                    agent-shell-sidebar--context-roots
                    (seq-filter
                     (lambda (binding)
                       (and (consp binding)
                            (string-match-p "\\`agent-\\(?:shell-sidebar\\|sidebar\\)-[^-]"
                                            (symbol-name (car binding)))))
                     (buffer-local-variables))) owners)
        (agent-shell-sidebar--cleanup))))
  (load "/Users/ducnguyen/.emacs.d/lisp/agent-shell-sidebar.elc" nil t)
  (load "/Users/ducnguyen/.emacs.d/lisp/agent-sidebar.elc" nil t)
  (dolist (owner owners)
    (with-current-buffer (car owner)
      (funcall (nth 1 owner))
      (dolist (binding (nth 6 owner))
        (set (make-local-variable (car binding)) (cdr binding)))
      (setq agent-shell-sidebar--marks (nth 2 owner)
            agent-shell-sidebar--collapsed (nth 3 owner)
            agent-shell-sidebar--filter (nth 4 owner)
            agent-shell-sidebar--context-roots (nth 5 owner))
      (agent-shell-sidebar--schedule-refresh)
      (agent-shell-sidebar-refresh))))

;; Evaluate only the changed startup declaration and the existing menu form.
(with-temp-buffer
  (insert-file-contents "/Users/ducnguyen/.emacs.d/lisp/package-declarations.el")
  (dolist (prefix '("(use-package agent-sidebar\n" "(defhydra hydra-submenu-agent-shell "))
    (goto-char (point-min))
    (search-forward prefix)
    (goto-char (- (point) (length prefix)))
    (eval (read (current-buffer)) t)))

(let* ((temporary (file-name-as-directory (make-temp-file "agent-sidebar-live-" t)))
       (root (expand-file-name "repo with space/" temporary))
       (store (expand-file-name "claude/" temporary))
       (buffer (generate-new-buffer " *agent sidebar live fixture*"))
       (result-file "/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/live-check.eldata")
       (started (float-time)) poller)
  (make-directory (expand-file-name ".agent-shell/transcripts/" root) t)
  (make-directory (expand-file-name (agent-sidebar--claude-encode-path root) store) t)
  (dotimes (i 36)
    (with-temp-file (expand-file-name (format ".agent-shell/transcripts/%03d.md" i) root)
      (insert (format "**Agent:** Fixture\n**Session ID:** md-%d\n**Model:** fixture-shell-model\n---\n## User\n\nLive shell fixture %d\n" i i)))
    (with-temp-file (expand-file-name (format "%s/%03d.jsonl" (agent-sidebar--claude-encode-path root) i) store)
      (insert (json-encode `((type . "user") (cwd . ,root) (timestamp . "2026-09-08T19:00:00Z")
                            (message . ((content . ,(format "Live CLI fixture %d" i)))))) "\n"
              (json-encode '((type . "assistant") (message . ((model . "fixture-cli-model"))))) "\n")))
  (with-current-buffer buffer
    (let ((agent-sidebar-refresh-timer nil)) (agent-sidebar-mode))
    (setq-local agent-sidebar-refresh-timer nil)
    (setq-local agent-sidebar-enabled-providers '(agent-shell claude-cli))
    (setq-local agent-sidebar-grouping '(package repo model))
    (setq-local agent-sidebar-claude-cli-projects-dir store)
    (setq-local agent-sidebar-parse-chunk-size 7)
    (setq-local agent-sidebar-parse-idle-delay 0.05)
    (cl-letf (((symbol-function 'agent-sidebar--project-roots) (lambda () (list root))))
      (agent-sidebar-refresh)))
  (setq poller
        (run-at-time
         0.1 0.1
         (lambda ()
           (when (or (not (buffer-live-p buffer))
                     (> (- (float-time) started) 15)
                     (with-current-buffer buffer
                       (and (null agent-sidebar--parse-timer)
                            (null agent-sidebar--parse-queue)
                            (null agent-shell-sidebar--render-iterator))))
             (unwind-protect
                 (let ((result
                        (append
                         (list :pid (emacs-pid) :emacs emacs-version :depth (recursion-depth)
                               :core-library (symbol-file 'agent-shell-sidebar--parse-tick 'defun)
                               :provider-library (symbol-file 'agent-sidebar-toggle-sidebar 'defun)
                               :compiled (byte-code-function-p (symbol-function 'agent-sidebar-toggle-sidebar))
                               :menu-key (lookup-key hydra-submenu-agent-shell/keymap (kbd "b"))
                               :elapsed (- (float-time) started))
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (let ((shell 0) (cli 0))
                               (save-excursion
                                 (goto-char (point-min))
                                 (while (search-forward "Live shell fixture " nil t) (cl-incf shell))
                                 (goto-char (point-min))
                                 (while (search-forward "Live CLI fixture " nil t) (cl-incf cli)))
                               (list :mode major-mode :parsed (hash-table-count agent-sidebar--parse-cache)
                                     :pending (length agent-sidebar--parse-queue)
                                     :render-error agent-shell-sidebar--render-error
                                     :shell-rows shell :cli-rows cli
                                     :both-model-groups
                                     (and (not (null (string-search "fixture-shell-model (36)" (buffer-string))))
                                          (not (null (string-search "fixture-cli-model (36)" (buffer-string))))))))))))
                   (with-temp-file result-file (prin1 result (current-buffer)) (insert "\n")))
               (cancel-timer poller)
               (when (buffer-live-p buffer) (kill-buffer buffer))
               (delete-directory temporary t)))))))

(provide 'agent-sidebar-live-check)
