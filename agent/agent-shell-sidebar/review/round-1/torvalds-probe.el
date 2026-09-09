;;; torvalds-probe.el --- Independent API/error-path probes -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-sidebar)

(message "PROBE ENVIRONMENT: Emacs %s; agent-shell %s" emacs-version agent-shell--version)

(defun torvalds-probe--write (root)
  (let ((file (expand-file-name ".agent-shell/transcripts/session.md" root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (format "# Agent Shell Transcript\n\n**Agent:** Fixture\n**Working Directory:** %s\n**Session ID:** fixture-session\n\n---\n\n## User\n\nTemporary fixture.\n" root)))
    file))

(ert-deftest torvalds-retries-header-after-read-permission-is-restored ()
  "Restored readability must recover without an unrelated content change."
  (let* ((root (file-name-as-directory (make-temp-file "sidebar-torvalds-read-" t)))
         (file (torvalds-probe--write root))
         (agent-shell-sidebar-refresh-timer nil)
         (agent-shell-sidebar--parse-cache (make-hash-table :test 'equal))
         (agent-shell-sidebar-extra-project-roots (list root))
         (projectile-known-projects nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-known-project-roots) (lambda () nil))
                  ((symbol-function 'agent-shell-buffers) (lambda () nil)))
          (with-temp-buffer
            (agent-shell-sidebar-mode)
            (set-file-modes file #o000)
            (should-not (file-readable-p file))
            (agent-shell-sidebar-refresh)
            (let* ((before (file-attributes file))
                   (unreadable (agent-shell-sidebar--ensure-parsed file)))
              (should (plist-get unreadable :error))
              (set-file-modes file #o600)
              (should (file-readable-p file))
              (let ((after (file-attributes file))
                    (fresh (agent-shell-sidebar--parse-header-from-file file)))
                (should (equal "fixture-session" (plist-get fresh :session-id)))
                (message "READABILITY: before=unreadable after=readable direct-parser-session=%S signature-unchanged=%S"
                         (plist-get fresh :session-id)
                         (equal (agent-shell-sidebar--signature before)
                                (agent-shell-sidebar--signature after))))
              (agent-shell-sidebar-refresh)
              (let ((recovered (agent-shell-sidebar--ensure-parsed file)))
                (message "RECOVERY: pending-after-refresh=%S cached-error-after-retry=%S"
                         (length agent-shell-sidebar--parse-queue)
                         (plist-get recovered :error))
                (should-not (plist-get recovered :error))))))
      (set-file-modes file #o600)
      (delete-directory root t))))

(ert-deftest torvalds-refuses-owned-transcript-through-project-path-alias ()
  "Directory aliases must not bypass the open-transcript deletion check."
  (let* ((base (file-name-as-directory (make-temp-file "sidebar-torvalds-alias-" t)))
         (root (expand-file-name "repo/" base))
         (alias (expand-file-name "alias/" base))
         (file (torvalds-probe--write root))
         (alias-file (expand-file-name ".agent-shell/transcripts/session.md" alias))
         (owner (generate-new-buffer " *torvalds fixture shell*"))
         (agent-shell-sidebar-refresh-timer nil)
         (agent-shell-sidebar--parse-cache (make-hash-table :test 'equal))
         (agent-shell-sidebar-extra-project-roots (list alias))
         (projectile-known-projects nil)
         (delete-by-moving-to-trash nil))
    (unwind-protect
        (progn
          (make-symbolic-link (directory-file-name root) (directory-file-name alias))
          ;; agent-shell-buffers' actual contract recognizes this minimal owner.
          ;; No agent process, hooks, or external service is started.
          (with-current-buffer owner
            (setq-local major-mode 'agent-shell-mode)
            (setq-local shell-maker--config 'fixture-config)
            (setq-local agent-shell--transcript-file file)
            (setq-local default-directory root))
          (should (memq owner (agent-shell-buffers)))
          (cl-letf (((symbol-function 'project-known-project-roots) (lambda () nil))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (with-temp-buffer
              (agent-shell-sidebar-mode)
              (agent-shell-sidebar-refresh)
              (should (member alias-file (cdr (assoc alias agent-shell-sidebar--groups))))
              (should (file-equal-p alias-file file))
              (puthash alias-file 'delete agent-shell-sidebar--marks)
              (message "ALIAS: discovered=%S same-file=%S exact-owner-path-match=%S owner-listed=%S"
                       t (file-equal-p alias-file file) (equal alias-file file)
                       (not (null (memq owner (agent-shell-buffers)))))
              (agent-shell-sidebar-execute)
              (message "DELETION: owned-file-exists=%S retained-mark=%S"
                       (file-exists-p file)
                       (gethash alias-file agent-shell-sidebar--marks))
              (should (file-exists-p file))
              (should (eq 'delete (gethash alias-file agent-shell-sidebar--marks))))))
      (kill-buffer owner)
      (delete-directory base t))))

(ert-deftest torvalds-control-preserves-owned-transcript-at-exact-path ()
  "The existing ownership check works when the path spelling is identical."
  (let* ((root (file-name-as-directory (make-temp-file "sidebar-torvalds-control-" t)))
         (file (torvalds-probe--write root))
         (owner (generate-new-buffer " *torvalds exact-path owner*"))
         (agent-shell-sidebar-refresh-timer nil)
         (agent-shell-sidebar--parse-cache (make-hash-table :test 'equal))
         (agent-shell-sidebar-extra-project-roots (list root))
         (projectile-known-projects nil)
         (delete-by-moving-to-trash nil))
    (unwind-protect
        (progn
          (with-current-buffer owner
            (setq-local major-mode 'agent-shell-mode)
            (setq-local shell-maker--config 'fixture-config)
            (setq-local agent-shell--transcript-file file)
            (setq-local default-directory root))
          (cl-letf (((symbol-function 'project-known-project-roots) (lambda () nil))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (with-temp-buffer
              (agent-shell-sidebar-mode)
              (puthash file 'delete agent-shell-sidebar--marks)
              (agent-shell-sidebar-execute)
              (message "CONTROL: exact-owner-path owned-file-exists=%S retained-mark=%S"
                       (file-exists-p file) (gethash file agent-shell-sidebar--marks))
              (should (file-exists-p file))
              (should (eq 'delete (gethash file agent-shell-sidebar--marks))))))
      (kill-buffer owner)
      (delete-directory root t))))

;;; torvalds-probe.el ends here
