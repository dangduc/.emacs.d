;;; load-live.el --- Switch the existing Emacs to the vendored package -*- lexical-binding: t; -*-
(require 'cl-lib)
(let ((vendor (expand-file-name "vendor/vegeta/" user-emacs-directory))
      (old-owners
       (seq-filter
        (lambda (buffer)
          (with-current-buffer buffer
            (or (derived-mode-p 'agent-sidebar-mode)
                (bound-and-true-p agent-sidebar--terminal-session)
                (bound-and-true-p agent-sidebar--agent-shell-file)
                (bound-and-true-p agent-sidebar--codex-session-home))))
        (buffer-list))))
  (add-to-list 'load-path vendor)
  (require 'vegeta)
  (cl-assert (file-in-directory-p (symbol-file 'vegeta-toggle-sidebar 'defun) vendor))
  ;; Evaluate only the changed package entry and agent-shell menu.
  (dolist (pattern '("^(use-package vegeta" "^[ \t]*(defhydra hydra-submenu-agent-shell"))
    (let ((form (with-temp-buffer
                  (insert-file-contents (expand-file-name "lisp/package-declarations.el" user-emacs-directory))
                  (goto-char (point-min))
                  (re-search-forward pattern)
                  (goto-char (match-beginning 0))
                  (read (current-buffer)))))
      (eval form t)))
  ;; Keep callback definitions if an existing session still depends on them.
  (when (and (featurep 'agent-sidebar) (null old-owners)
             (null (bound-and-true-p agent-sidebar--timer-buffers))
             (null (file-dependents (feature-file 'agent-sidebar))))
    (unload-feature 'agent-sidebar))
  ;; Unloading restores autoloads created by the earlier init entry.
  (unless (featurep 'agent-sidebar)
    (dolist (command '(agent-sidebar-toggle-sidebar agent-sidebar-show-sidebar
                      agent-sidebar-hide-sidebar agent-sidebar-jump-to-sidebar))
      (when (and (fboundp command) (autoloadp (symbol-function command)))
        (fmakunbound command))))
  (let ((result
         (list :pid (emacs-pid) :depth (recursion-depth)
               :vegeta-feature (featurep 'vegeta)
               :library (locate-library "vegeta")
               :definition (symbol-file 'vegeta-toggle-sidebar 'defun)
               :menu (lookup-key hydra-submenu-agent-shell/keymap "b")
               :old-feature (featurep 'agent-sidebar)
               :old-command (fboundp 'agent-sidebar-toggle-sidebar)
               :retained-old-owner-count (length old-owners)
               :providers vegeta-enabled-providers)))
    (with-temp-file (expand-file-name "agent/vegeta-install/live-result.eldata" user-emacs-directory)
      (prin1 result (current-buffer)) (insert "\n"))
    result))
