;;; ousterhout-probe.el --- State ownership review probes -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-sidebar)

(defun ousterhout--write (root &optional agent)
  "Write an isolated transcript under ROOT for AGENT."
  (let ((file (expand-file-name ".agent-shell/transcripts/session.md" root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (format "**Agent:** %s\n**Session ID:** old-session\n**Working Directory:** %s\n---\n## User\nFirst prompt\n"
                      (or agent "Claude") root)))
    file))

(defmacro ousterhout--project (&rest body)
  "Evaluate BODY with a temporary project and an isolated discovery scope."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "ousterhout-sidebar-" t)))
          (default-directory root)
          (agent-shell-sidebar-extra-project-roots (list root))
          (projectile-known-projects nil)
          (agent-shell-sidebar-refresh-timer nil)
          (agent-shell-sidebar--parse-cache (make-hash-table :test 'equal)))
     (unwind-protect
         (cl-letf (((symbol-function 'project-known-project-roots) (lambda () nil))
                   ((symbol-function 'agent-shell-buffers) (lambda () nil))
                   ((symbol-function 'input-pending-p) (lambda () nil)))
           ,@body)
       (delete-directory root t))))

(defun ousterhout--drain ()
  "Parse the small test fixture without entering an interactive event loop."
  (let ((remaining 20))
    (while (and agent-shell-sidebar--parse-queue (> remaining 0))
      (setq remaining (1- remaining))
      (agent-shell-sidebar--parse-tick))
    (should-not agent-shell-sidebar--parse-queue)))

(defun ousterhout--goto-file (file)
  "Select FILE's row."
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (equal file (get-text-property (point) 'agent-shell-sidebar-file))))
    (forward-line 1))
  (should-not (eobp)))

(ert-deftest ousterhout-live-request-id-is-not-current-after-fallback ()
  ;; Run the real agent-shell fallback branch and real session/new success
  ;; callback.  Only the transport and presentation side effects are replaced.
  (with-temp-buffer
    (let* ((owner (current-buffer))
           (config '((:mode-line-name . "Claude")))
           (default-directory "/private/tmp/")
           (agent-shell-cwd-function (lambda () "/private/tmp/"))
           request-method initialized)
      (setq-local major-mode 'agent-shell-mode)
      (setq-local agent-shell--state
                  (agent-shell--make-state :buffer owner :agent-config config))
      (map-put! agent-shell--state :resume-session-id "old-session")
      (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list owner)))
                ((symbol-function 'agent-shell--update-bootstrapping-fragment) #'ignore)
                ((symbol-function 'agent-shell--emit-event) #'ignore)
                ((symbol-function 'agent-shell--save-config-options) #'ignore)
                ((symbol-function 'agent-shell--update-header-and-mode-line) #'ignore)
                ((symbol-function 'agent-shell--display-session-options) #'ignore)
                ((symbol-function 'agent-shell--send-request)
                 (lambda (&rest args)
                   (setq request-method (map-elt (plist-get args :request) :method))
                   (funcall (plist-get args :on-success)
                            '((sessionId . "new-session"))))))
        ;; The pending marker is a valid identity before startup completes.
        (should (eq owner (agent-shell-sidebar--live-buffer-for-session
                           "old-session" "Claude" default-directory)))
        (agent-shell--initiate-session
         :shell-buffer owner :on-session-init (lambda () (setq initialized t)))
        (should initialized)
        (should (equal request-method "session/new"))
        (should (equal "new-session" (map-nested-elt agent-shell--state '(:session :id))))
        (should (eq owner (agent-shell-sidebar--live-buffer-for-session
                           "new-session" "Claude" default-directory)))
        (message "OUSTERHOUT-1: actual-session=%S retained-request=%S old-row-reuses-new=%S"
                 (map-nested-elt agent-shell--state '(:session :id))
                 (map-elt agent-shell--state :resume-session-id)
                 (eq owner (agent-shell-sidebar--live-buffer-for-session
                            "old-session" "Claude" default-directory)))
        (should-not (agent-shell-sidebar--live-buffer-for-session
                     "old-session" "Claude" default-directory))))))

(ert-deftest ousterhout-global-cache-preserves-other-buffer-metadata ()
  (ousterhout--project
    (let ((file (ousterhout--write root))
          (one (generate-new-buffer " *ousterhout sidebar one*"))
          (two (generate-new-buffer " *ousterhout sidebar two*")))
      (unwind-protect
          (progn
            (with-current-buffer one
              (agent-shell-sidebar-mode)
              (agent-shell-sidebar-refresh)
              (ousterhout--drain)
              (should (string-match-p "First prompt" (buffer-string))))
            ;; A live transcript grows; its header remains identical.
            (write-region "\n## Agent\nA new response\n" nil file t 'silent)
            (with-current-buffer two
              (agent-shell-sidebar-mode)
              (agent-shell-sidebar-refresh)
              (ousterhout--drain)
              (should (string-match-p "First prompt" (buffer-string))))
            (with-current-buffer one
              (agent-shell-sidebar--redraw)
              (message "OUSTERHOUT-2: first-sidebar=%S pending=%S timer=%S"
                       (buffer-substring-no-properties (point-min) (point-max))
                       agent-shell-sidebar--parse-queue agent-shell-sidebar--parse-timer)
              (should-not agent-shell-sidebar--parse-queue)
              (should-not agent-shell-sidebar--parse-timer)
              (should (string-match-p "First prompt" (buffer-string)))))
        (kill-buffer one)
        (kill-buffer two)))))

(ert-deftest ousterhout-substring-agent-resolution-reuses-started-session ()
  (ousterhout--project
    (let* ((file (ousterhout--write root "Claude Code"))
           (config '((:mode-line-name . "Claude") (:buffer-name . "Claude")))
           (agent-shell-agent-configs (list config))
           (starts 0)
           shells)
      (unwind-protect
          (with-temp-buffer
            (agent-shell-sidebar-mode)
            (agent-shell-sidebar-refresh)
            (ousterhout--drain)
            (ousterhout--goto-file file)
            (should (equal config (agent-shell-sidebar--config-for-agent-name "Claude Code")))
            (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () shells))
                      ((symbol-function 'agent-shell-sidebar--pop-to) #'ignore)
                      ((symbol-function 'agent-shell-sidebar--start)
                       (lambda (directory selected-config session-id)
                         (cl-incf starts)
                         (let ((buffer (generate-new-buffer " *ousterhout mock shell*")))
                           (push buffer shells)
                           (with-current-buffer buffer
                             (setq-local default-directory directory)
                             (setq-local agent-shell--state
                                         (agent-shell--make-state :buffer buffer :agent-config selected-config))
                             (map-put! agent-shell--state :resume-session-id session-id))
                           buffer))))
              (agent-shell-sidebar-visit)
              (agent-shell-sidebar-visit))
            (message "OUSTERHOUT-3: recorded-agent=Claude Code resolved-agent=Claude visits=2 starts=%d"
                     starts)
            (should (= starts 1)))
        (dolist (buffer shells) (kill-buffer buffer))))))

;;; ousterhout-probe.el ends here
