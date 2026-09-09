;;; live-final.el --- Final runtime identity and cleanup evidence -*- lexical-binding: t; -*-
(let ((result
       (list :pid (emacs-pid) :version emacs-version :depth (recursion-depth)
             :frontend (symbol-file 'agent-sidebar-new-session 'defun)
             :core (symbol-file 'agent-shell-sidebar--start 'defun)
             :new-helpers-compiled
             (cl-every (lambda (name) (byte-code-function-p (symbol-function name)))
                       '(agent-sidebar--bounded-preview agent-sidebar--codex-session-config
                         agent-sidebar--codex-owned-file-p agent-sidebar--new-context-for-group))
             :preview-bound (= 200 (length (agent-sidebar--bounded-preview (make-string 201 ?x))))
             :providers agent-sidebar-enabled-providers
             :A (lookup-key agent-sidebar-mode-map (kbd "A"))
             :N (lookup-key agent-sidebar-mode-map (kbd "N"))
             :codex (executable-find agent-sidebar-codex-cli-command)
             :codex-acp (executable-find "codex-acp")
             :ghostel (fboundp 'ghostel-exec)
             :fixture-buffers
             (seq-filter (lambda (buffer) (string-match-p "codex sidebar review live fixture\\|codex-resume: 12345678" (buffer-name buffer)))
                         (buffer-list))
             :fixture-timer-buffers
             (seq-filter (lambda (buffer) (and (buffer-live-p buffer)
                                              (string-match-p "codex sidebar review live fixture" (buffer-name buffer))))
                         agent-shell-sidebar--timer-buffers))))
  (with-temp-file "/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/live-final.eldata"
    (prin1 result (current-buffer)) (insert "\n"))
  result)
