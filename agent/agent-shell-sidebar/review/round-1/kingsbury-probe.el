;;; kingsbury-probe.el --- Isolated failure schedules -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'timer)

(defmacro kingsbury--fixture (&rest body)
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "kingsbury-sidebar-" t)))
          (default-directory root)
          (agent-shell-sidebar-refresh-timer nil)
          (agent-shell-sidebar-extra-project-roots (list root))
          (agent-shell-sidebar--parse-cache (make-hash-table :test 'equal))
          (projectile-known-projects nil)
          (agent-shell-sidebar-parse-chunk-size 1))
     (unwind-protect
         (cl-letf (((symbol-function 'project-known-project-roots) (lambda () nil))
                   ((symbol-function 'agent-shell-buffers) (lambda () nil)))
           (with-temp-buffer
             (agent-shell-sidebar-mode)
             ,@body))
       (delete-directory root t))))

(defun kingsbury--write (root name &optional session)
  (let ((file (expand-file-name (concat ".agent-shell/transcripts/" name ".md") root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (format "**Agent:** Mock\n**Session ID:** %s\n---\n## User\n\n%s\n"
                      (or session name) name)))
    file))

(ert-deftest kingsbury-cancelled-timer-does-not-run ()
  "Control: Emacs rejects a timer cancelled before dispatch."
  (kingsbury--fixture
    (let ((file (kingsbury--write root "cancelled")))
      (agent-shell-sidebar-refresh)
      (let ((timer agent-shell-sidebar--parse-timer))
        (should (memq timer timer-idle-list))
        (agent-shell-sidebar--cleanup)
        (timer-event-handler timer)
        (should-not agent-shell-sidebar--parse-queue)
        (should-not agent-shell-sidebar--parse-timer)
        (should-not (gethash file agent-shell-sidebar--parse-cache))
        (message "Cancelled timer: no callback, no cache write, no queue or timer.")))))

(ert-deftest kingsbury-mode-change-cancels-ready-timer ()
  "Control: mode change stops a previously scheduled parse callback."
  (kingsbury--fixture
    (let ((file (kingsbury--write root "mode-change")))
      (agent-shell-sidebar-refresh)
      (let ((timer agent-shell-sidebar--parse-timer))
        (fundamental-mode)
        (timer-event-handler timer)
        (should-not (gethash file agent-shell-sidebar--parse-cache))
        (should-not agent-shell-sidebar--parse-timer)
        (message "Mode change: ready timer cancelled without a cache write.")))))

(ert-deftest kingsbury-read-race-is-rechecked-on-next-activation ()
  "Control: a write between stat and read does not survive the next stat."
  (kingsbury--fixture
    (let* ((file (kingsbury--write root "race" "old"))
           (read-file (symbol-function 'insert-file-contents))
           (changed nil))
      (cl-letf (((symbol-function 'insert-file-contents)
                 (lambda (path &rest args)
                   (when (and (equal path file) (not changed))
                     (setq changed t)
                     (kingsbury--write root "race" "a-new-longer-session"))
                   (apply read-file path args))))
        (should (equal "a-new-longer-session"
                       (plist-get (agent-shell-sidebar--ensure-parsed file) :session-id))))
      ;; The cache receives the pre-read signature, but the next activation
      ;; obtains fresh attributes and repairs that signature.
      (should-not (equal (plist-get (gethash file agent-shell-sidebar--parse-cache) :signature)
                         (agent-shell-sidebar--signature (file-attributes file))))
      (should (equal "a-new-longer-session"
                     (plist-get (agent-shell-sidebar--ensure-parsed file) :session-id)))
      (should (equal (plist-get (gethash file agent-shell-sidebar--parse-cache) :signature)
                     (agent-shell-sidebar--signature (file-attributes file))))
      (message "Read race: next activation repaired the old signature and retained the new session."))))

(ert-deftest kingsbury-transient-read-hook-error-keeps-work-scheduled ()
  "Regression: one transient insertion-hook error must not strand the queue."
  (kingsbury--fixture
    (let* ((one (kingsbury--write root "one"))
           (two (kingsbury--write root "two"))
           (fault-count 0))
      (agent-shell-sidebar-refresh)
      ;; Make the order independent of filesystem timestamp granularity.
      (setq agent-shell-sidebar--parse-queue (list one two))
      (let ((debug-on-error nil)
            (after-insert-file-functions
             (list (lambda (_length)
                     (cl-incf fault-count)
                     (error "Injected transient after-insert-file-functions failure")))))
        ;; Exercise the actual dispatcher, actual file read, and actual hook.
        ;; Only fault delivery is synthetic. No live Emacs or keyboard input.
        (timer-event-handler agent-shell-sidebar--parse-timer))
      (should (= fault-count 1))
      (should (file-readable-p one))
      (should (file-readable-p two))
      (message "Transient hook error: first file cached=%S, queued=%S; remaining=%S; timer=%S"
               (not (null (gethash one agent-shell-sidebar--parse-cache)))
               (not (null (member one agent-shell-sidebar--parse-queue)))
               (mapcar #'file-name-nondirectory agent-shell-sidebar--parse-queue)
               agent-shell-sidebar--parse-timer)
      ;; Desired liveness: the remaining work has an active timer after the
      ;; transient fault disappears. The frozen source fails this assertion.
      (should (timerp agent-shell-sidebar--parse-timer)))))

;;; kingsbury-probe.el ends here
