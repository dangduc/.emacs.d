;;; metadata-original-probes.el --- Original probes for metadata fixes -*- lexical-binding: t; -*-
(let ((round (expand-file-name "../round-1/"
                               (file-name-directory (or load-file-name buffer-file-name)))))
  (dolist (name '("ousterhout-probe.el" "torvalds-probe.el" "contrarian-evidence-probe.el"
                  "contrarian-workflow.el"))
    (load (expand-file-name name round) nil t)))
;; These independent tests belong to the other fix owners.  All assertions
;; in the metadata probes and the existing controls run unchanged.
(dolist (test '(ousterhout-live-request-id-is-not-current-after-fallback
                ousterhout-substring-agent-resolution-reuses-started-session
                torvalds-refuses-owned-transcript-through-project-path-alias
                workflow-disable-refresh-before-pending-callback
                workflow-toggle-works-after-sidebar-buffer-switched-to-normal-window))
  (ert-delete-test test))
