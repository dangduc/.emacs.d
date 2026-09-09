;;; preflight.el --- Inspect sidebar lifecycle state -*- lexical-binding: t; -*-
(list :pid (emacs-pid) :depth (recursion-depth)
      :old-feature (featurep 'agent-sidebar)
      :old-owner-count
      (length
       (seq-filter
        (lambda (buffer)
          (with-current-buffer buffer
            (or (derived-mode-p 'agent-sidebar-mode)
                (bound-and-true-p agent-sidebar--terminal-session)
                (bound-and-true-p agent-sidebar--agent-shell-file)
                (bound-and-true-p agent-sidebar--codex-session-home))))
        (buffer-list)))
      :old-timer-count (length (bound-and-true-p agent-sidebar--timer-buffers))
      :old-dependents (when (featurep 'agent-sidebar) (file-dependents (feature-file 'agent-sidebar)))
      :menu (lookup-key hydra-submenu-agent-shell/keymap "b")
      :old-config
      (delq nil
            (mapcar (lambda (symbol)
                      (when (boundp symbol) (list symbol (symbol-value symbol))))
                    '(agent-sidebar-name agent-sidebar-width agent-sidebar-refresh-timer))))
