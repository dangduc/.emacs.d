;;; live-check.el --- Check sidebar loading and real idle parsing -*- lexical-binding: t; -*-
(require 'cl-lib)
(let ((owners
       (delq nil
             (mapcar
              (lambda (buffer)
                (with-current-buffer buffer
                  (when (derived-mode-p 'agent-shell-sidebar-mode)
                    (list buffer
                          agent-shell-sidebar--marks
                          agent-shell-sidebar--collapsed
                          agent-shell-sidebar--filter
                          agent-shell-sidebar--context-roots
                          (seq-filter
                           (lambda (binding)
                             (and (consp binding)
                                  (string-prefix-p "agent-shell-sidebar-" (symbol-name (car binding)))
                                  (not (string-prefix-p "agent-shell-sidebar--" (symbol-name (car binding))))))
                           (buffer-local-variables))))))
              (buffer-list)))))
  (dolist (owner owners)
    (with-current-buffer (car owner) (agent-shell-sidebar--cleanup)))
  (load "/Users/ducnguyen/.emacs.d/lisp/agent-shell-sidebar.elc" nil t)
  (dolist (owner owners)
    (with-current-buffer (car owner)
      (agent-shell-sidebar-mode)
      (dolist (binding (nth 5 owner))
        (set (make-local-variable (car binding)) (cdr binding)))
      (setq agent-shell-sidebar--marks (nth 1 owner)
            agent-shell-sidebar--collapsed (nth 2 owner)
            agent-shell-sidebar--filter (nth 3 owner)
            agent-shell-sidebar--context-roots (nth 4 owner))
      (setq-local window-size-fixed agent-shell-sidebar-window-fixed)
      (agent-shell-sidebar--schedule-refresh)
      (agent-shell-sidebar-refresh))))
(let* ((root (file-name-as-directory (make-temp-file "sidebar-live-idle-" t)))
       (buffer (generate-new-buffer " *sidebar idle check*"))
       (started (float-time))
       (result-file "/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/live-check.eldata")
       poller paths)
  (make-directory (expand-file-name ".agent-shell/transcripts/" root) t)
  (dotimes (i 71)
    (let ((file (expand-file-name (format ".agent-shell/transcripts/%03d.md" i) root)))
      (with-temp-file file
        (insert (format "**Agent:** Fixture\n**Session ID:** %d\n---\n## User (now)\n\nIdle fixture %d\n" i i)))
      (push file paths)))
  (with-current-buffer buffer
    (let ((agent-shell-sidebar-refresh-timer nil)) (agent-shell-sidebar-mode))
    (setq-local agent-shell-sidebar--parse-cache (make-hash-table :test 'equal))
    (setq-local agent-shell-sidebar--file-info (make-hash-table :test 'equal))
    (setq-local agent-shell-sidebar-parse-chunk-size 7)
    (setq-local agent-shell-sidebar-parse-idle-delay 0.05)
    (dolist (file paths) (puthash file (file-attributes file) agent-shell-sidebar--file-info))
    (setq agent-shell-sidebar--groups (list (cons root paths)))
    (agent-shell-sidebar--queue-uncached paths)
    (agent-shell-sidebar--redraw)
    (agent-shell-sidebar--start-parse-timer))
  (setq poller
        (run-at-time
         0.1 0.1
         (lambda ()
           (when (or (not (buffer-live-p buffer))
                     (> (- (float-time) started) 12)
                     (and (null (buffer-local-value 'agent-shell-sidebar--parse-queue buffer))
                          (null (buffer-local-value 'agent-shell-sidebar--parse-timer buffer))
                          (null (buffer-local-value 'agent-shell-sidebar--render-iterator buffer))))
             (unwind-protect
                 (let ((result (list :pid (emacs-pid) :emacs emacs-version
                                     :library (symbol-file 'agent-shell-sidebar-toggle-sidebar 'defun)
                                     :compiled (byte-code-function-p (symbol-function 'agent-shell-sidebar-toggle-sidebar))
                                     :queue-ownership-guard (fboundp 'agent-shell-sidebar--current-parse-p)
                                     :agent-version agent-shell--version
                                     :command (commandp 'agent-shell-sidebar-toggle-sidebar)
                                     :menu-key (lookup-key hydra-submenu-agent-shell/keymap (kbd "b"))
                                     :parsed (and (buffer-live-p buffer)
                                                  (hash-table-count (buffer-local-value 'agent-shell-sidebar--parse-cache buffer)))
                                     :pending (and (buffer-live-p buffer)
                                                   (length (buffer-local-value 'agent-shell-sidebar--parse-queue buffer)))
                                     :render-error (and (buffer-live-p buffer)
                                                        (buffer-local-value 'agent-shell-sidebar--render-error buffer))
                                     :rendering (and (buffer-live-p buffer)
                                                     (not (null (buffer-local-value 'agent-shell-sidebar--render-iterator buffer))))
                                     :rendered (and (buffer-live-p buffer)
                                                    (with-current-buffer buffer
                                                      (save-excursion
                                                        (goto-char (point-min))
                                                        (let ((count 0))
                                                          (while (re-search-forward "Idle fixture [0-9]+" nil t)
                                                            (cl-incf count))
                                                          count))))
                                     :elapsed (- (float-time) started)
                                     :agent-buffers (length (agent-shell-buffers))
                                     :recursion-depth (recursion-depth))))
                   (with-temp-file result-file (prin1 result (current-buffer))))
               (cancel-timer poller)
               (when (buffer-live-p buffer) (kill-buffer buffer))
               (delete-directory root t)))))))
;;; live-check.el ends here
