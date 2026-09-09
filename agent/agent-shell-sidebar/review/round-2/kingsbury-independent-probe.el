;;; kingsbury-independent-probe.el --- Independent event histories -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'timer)

(princ (format "SOURCE=%s\n" (symbol-file 'agent-shell-sidebar--parse-tick 'defun)))

(defun ki2--write (root name agent preview &optional stamp)
  (let ((file (expand-file-name (concat ".agent-shell/transcripts/" name ".md") root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (format "**Agent:** %s\n---\n## User\n\n%s\n" agent preview)))
    (set-file-times file (seconds-to-time (or stamp 1000000)))
    file))

(defmacro ki2--fixture (count &rest body)
  (declare (indent 1))
  `(let* ((root (file-name-as-directory (make-temp-file "sidebar-ki2-" t)))
          (default-directory root)
          (agent-shell-sidebar-extra-project-roots (list root))
          (agent-shell-sidebar-refresh-timer nil)
          (agent-shell-sidebar-parse-chunk-size 1)
          (agent-shell-sidebar--timer-buffers nil)
          (post-command-hook nil)
          (projectile-known-projects nil)
          (owner (generate-new-buffer " *Independent sidebar owner*")))
     (unwind-protect
         (cl-letf (((symbol-function 'project-known-project-roots) (lambda () nil))
                   ((symbol-function 'agent-shell-buffers) (lambda () nil))
                   ((symbol-function 'input-pending-p) (lambda () nil)))
           (dotimes (i ,count)
             (ki2--write root (format "%04d" i) "Original" (format "Original %04d" i)))
           (with-current-buffer owner
             (agent-shell-sidebar-mode)
             (agent-shell-sidebar-refresh)
             ,@body))
       (when (buffer-live-p owner) (kill-buffer owner))
       (delete-directory root t))))

(defun ki2--dispatch ()
  (should (memq agent-shell-sidebar--parse-timer timer-idle-list))
  (timer-event-handler agent-shell-sidebar--parse-timer))

(defun ki2--drain ()
  (let ((count 0))
    (while (and (timerp agent-shell-sidebar--parse-timer) (< count 500))
      (ki2--dispatch)
      (cl-incf count))
    (should (< count 500))
    (should-not agent-shell-sidebar--parse-queue)
    (should-not agent-shell-sidebar--parse-timer)
    (should-not agent-shell-sidebar--render-iterator)
    (should-not agent-shell-sidebar--render-dirty)
    (should-not agent-shell-sidebar--render-error)
    count))

(ert-deftest ki2-refresh-during-read-retains-newly-discovered-work ()
  "A read hook can refresh discovery before the outer queue head completes."
  (ki2--fixture 12
    (let ((old-head (car agent-shell-sidebar--parse-queue))
          triggered fresh queued-after-refresh)
      (let ((after-insert-file-functions
             (list
              (lambda (_size)
                (unless triggered
                  (setq triggered t
                        fresh (ki2--write root "fresh" "Fresh" "Freshly discovered row" 2000000))
                  (with-current-buffer owner
                    (agent-shell-sidebar-refresh)
                    (setq queued-after-refresh (copy-sequence agent-shell-sidebar--parse-queue))))
                nil))))
        (ki2--dispatch))
      (should triggered)
      (should (equal fresh (car queued-after-refresh)))
      (let ((remaining-after-read (copy-sequence agent-shell-sidebar--parse-queue)))
        (ki2--drain)
        (princ (format "READ-REFRESH old-head=%S new-head=%S new-file-retained=%S parsed-new=%S pending=%S visible-new=%S\n"
                       old-head (car queued-after-refresh)
                       (member fresh remaining-after-read)
                       (agent-shell-sidebar--cached-header fresh)
                       agent-shell-sidebar--pending-count
                       (string-search "Freshly discovered row" (buffer-string))))
        (should (equal "Fresh" (plist-get (agent-shell-sidebar--cached-header fresh) :agent)))
        (should (string-search "Freshly discovered row" (buffer-string)))))))

(ert-deftest ki2-discovery-error-preserves-stage-until-next-refresh-recovers ()
  "A failed automatic scan is recoverable while an existing stage is incomplete."
  (ki2--fixture 320
    (ki2--dispatch)
    (should agent-shell-sidebar--render-iterator)
    (save-window-excursion
      (set-window-buffer (selected-window) owner)
      (let ((agent-shell-sidebar-refresh-timer 30)
            (collect (symbol-function 'agent-shell-sidebar--collect))
            (stage agent-shell-sidebar--render-buffer)
            (old-parse agent-shell-sidebar--parse-timer)
            (before agent-shell-sidebar--pending-count)
            (scans 0))
        (agent-shell-sidebar--schedule-refresh)
        (cl-letf (((symbol-function 'agent-shell-sidebar--collect)
                   (lambda ()
                     (cl-incf scans)
                     (if (= scans 1) (error "Injected transient discovery error")
                       (funcall collect)))))
          (timer-event-handler agent-shell-sidebar--refresh-timer-object)
          (should (buffer-live-p stage))
          (should (= before agent-shell-sidebar--pending-count))
          (should (memq agent-shell-sidebar--refresh-timer-object timer-idle-list))
          (princ (format "DISCOVERY-ERROR pending=%S stage-live=%S parse-timer=%S automatic-retry-owned=%S\n"
                         agent-shell-sidebar--pending-count (buffer-live-p stage)
                         (timerp agent-shell-sidebar--parse-timer)
                         (memq agent-shell-sidebar--refresh-timer-object timer-idle-list)))
          (timer-event-handler agent-shell-sidebar--refresh-timer-object))
        (should (= scans 2))
        (should-not (buffer-live-p stage))
        (let ((pending agent-shell-sidebar--pending-count)
              (new-parse agent-shell-sidebar--parse-timer))
          (timer-event-handler old-parse)
          (should (= pending agent-shell-sidebar--pending-count))
          (should (eq new-parse agent-shell-sidebar--parse-timer)))
        (ki2--drain)
        (should (string-search "Original 0319" (buffer-string)))
        (princ "DISCOVERY-RECOVERY all headers and staging completed; stale timer had no effect.\n")))))

(ert-deftest ki2-recreated-buffer-name-does-not-transfer-old-timer-ownership ()
  "Cancelled callbacks retain the old buffer object when its name is reused."
  (ki2--fixture 2
    (let ((name (buffer-name owner))
          (agent-shell-sidebar-refresh-timer 30)
          (old-parse agent-shell-sidebar--parse-timer)
          old-refresh replacement)
      (agent-shell-sidebar--schedule-refresh)
      (setq old-refresh agent-shell-sidebar--refresh-timer-object)
      (kill-buffer owner)
      (setq replacement (generate-new-buffer name))
      (unwind-protect
          (with-current-buffer replacement
            (agent-shell-sidebar-mode)
            (agent-shell-sidebar-refresh)
            (should (equal name (buffer-name)))
            (let ((new-parse agent-shell-sidebar--parse-timer)
                  (new-refresh agent-shell-sidebar--refresh-timer-object)
                  (pending agent-shell-sidebar--pending-count))
              (timer-event-handler old-parse)
              (timer-event-handler old-refresh)
              (should (eq new-parse agent-shell-sidebar--parse-timer))
              (should (eq new-refresh agent-shell-sidebar--refresh-timer-object))
              (should (= pending agent-shell-sidebar--pending-count)))
            (ki2--drain)
            (should (string-search "Original 0001" (buffer-string)))
            (should (memq replacement agent-shell-sidebar--timer-buffers))
            (should-not (memq owner agent-shell-sidebar--timer-buffers))
            (princ "NAME-REUSE old parse and refresh callbacks left the replacement owner unchanged.\n"))
        (when (buffer-live-p replacement) (kill-buffer replacement))))))

(ert-deftest ki2-refresh-after-read-retains-newly-discovered-work ()
  "The same discovery update after the read completes preserves every header."
  (ki2--fixture 12
    (ki2--dispatch)
    (let ((fresh (ki2--write root "fresh" "Fresh" "Freshly discovered row" 2000000)))
      (agent-shell-sidebar-refresh)
      (should (equal fresh (car agent-shell-sidebar--parse-queue)))
      (ki2--drain)
      (should (equal "Fresh" (plist-get (agent-shell-sidebar--cached-header fresh) :agent)))
      (should (string-search "Freshly discovered row" (buffer-string)))
      (should (= 0 agent-shell-sidebar--pending-count))
      (princ "AFTER-READ-REFRESH new metadata and preview published; pending=0.\n"))))

(add-hook 'kill-emacs-hook
          (lambda ()
            (princ (format "SOURCE-AFTER=%s\n"
                           (symbol-file 'agent-shell-sidebar--parse-tick 'defun)))))
