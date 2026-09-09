;;; reentrancy-tests.el --- Queue ownership across file hooks -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'timer)

(defun reentrancy--write (root name &optional agent stamp)
  (let ((file (expand-file-name (concat ".agent-shell/transcripts/" name ".md") root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (format "**Agent:** %s\n---\n## User\n\nMessage %s\n" (or agent "Original") name)))
    (set-file-times file (seconds-to-time (or stamp 1000000)))
    file))

(defmacro reentrancy--fixture (&rest body)
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "sidebar-reentrancy-" t)))
          (default-directory root)
          (agent-shell-sidebar-extra-project-roots (list root))
          (agent-shell-sidebar-refresh-timer nil)
          (agent-shell-sidebar-parse-chunk-size 1)
          (agent-shell-sidebar--timer-buffers nil)
          (post-command-hook nil)
          (projectile-known-projects nil)
          (owner (generate-new-buffer " *Reentrancy owner*")))
     (unwind-protect
         (cl-letf (((symbol-function 'project-known-project-roots) (lambda () nil))
                   ((symbol-function 'agent-shell-buffers) (lambda () nil))
                   ((symbol-function 'input-pending-p) (lambda () nil)))
           (dotimes (i 12) (reentrancy--write root (format "%04d" i)))
           (with-current-buffer owner
             (agent-shell-sidebar-mode)
             (agent-shell-sidebar-refresh)
             ,@body))
       (when (buffer-live-p owner) (kill-buffer owner))
       (delete-directory root t))))

(defun reentrancy--dispatch ()
  (should (memq agent-shell-sidebar--parse-timer timer-idle-list))
  (timer-event-handler agent-shell-sidebar--parse-timer))

(defun reentrancy--finish ()
  (let ((callbacks 0))
    (while (and agent-shell-sidebar--parse-timer (< callbacks 100))
      (reentrancy--dispatch)
      (cl-incf callbacks))
    (should-not agent-shell-sidebar--parse-timer)
    (should-not agent-shell-sidebar--parse-queue)
    (should (= 0 agent-shell-sidebar--pending-count))
    (should-not agent-shell-sidebar--render-iterator)
    (should-not agent-shell-sidebar--render-dirty)
    (should-not agent-shell-sidebar--render-error)))

(ert-deftest reentrancy-refresh-retains-old-head-for-new-generation ()
  (reentrancy--fixture
    (let ((head (car agent-shell-sidebar--parse-queue))
          (old-generation agent-shell-sidebar--parse-generation)
          refreshed replacement-timer)
      (let ((after-insert-file-functions
             (list (lambda (_)
                     (unless refreshed
                       (setq refreshed t)
                       (with-current-buffer owner
                         (agent-shell-sidebar-refresh)
                         (setq replacement-timer agent-shell-sidebar--parse-timer)))
                     nil))))
        (reentrancy--dispatch))
      (should refreshed)
      (should-not (eq old-generation agent-shell-sidebar--parse-generation))
      (should (equal head (car agent-shell-sidebar--parse-queue)))
      (should (= 12 agent-shell-sidebar--pending-count))
      (should (eq replacement-timer agent-shell-sidebar--parse-timer))
      (should-not (agent-shell-sidebar--cached-header head))
      (should-not agent-shell-sidebar--render-iterator)
      (reentrancy--finish)
      (should (string-search "Message 0011" (buffer-string))))))

(ert-deftest reentrancy-error-after-refresh-does-not-charge-new-retry-budget ()
  (reentrancy--fixture
    (let ((head (car agent-shell-sidebar--parse-queue)) refreshed)
      (let ((after-insert-file-functions
             (list (lambda (_)
                     (unless refreshed
                       (setq refreshed t)
                       (with-current-buffer owner (agent-shell-sidebar-refresh))
                       (error "Error in obsolete read"))))))
        (reentrancy--dispatch))
      (should (= 12 agent-shell-sidebar--pending-count))
      (should-not agent-shell-sidebar--parse-retry-file)
      (let ((after-insert-file-functions
             (list (lambda (_) (error "First error in replacement read")))))
        (reentrancy--dispatch))
      (should (equal head agent-shell-sidebar--parse-retry-file))
      (should-not (agent-shell-sidebar--cached-header head))
      (should (= 12 agent-shell-sidebar--pending-count))
      (reentrancy--finish)
      (should-not (plist-get (agent-shell-sidebar--cached-header head) :error)))))

(ert-deftest reentrancy-mode-change-during-read-cannot-recreate-work ()
  (reentrancy--fixture
    (let ((after-insert-file-functions
           (list (lambda (_)
                   (with-current-buffer owner (fundamental-mode))
                   nil))))
      (reentrancy--dispatch))
    (should (eq major-mode 'fundamental-mode))
    (should-not agent-shell-sidebar--parse-cache)
    (should-not agent-shell-sidebar--parse-queue)
    (should-not agent-shell-sidebar--parse-timer)
    (should-not agent-shell-sidebar--render-buffer)
    (should-not agent-shell-sidebar--render-dirty)
    (should-not agent-shell-sidebar--parse-retry-file)
    (should-not (memq owner agent-shell-sidebar--timer-buffers))))

(ert-deftest reentrancy-mode-reinitialization-keeps-replacement-work ()
  (reentrancy--fixture
    (let ((old-cache agent-shell-sidebar--parse-cache)
          reinitialized replacement-timer)
      (let ((after-insert-file-functions
             (list (lambda (_)
                     (unless reinitialized
                       (setq reinitialized t)
                       (with-current-buffer owner
                         (fundamental-mode)
                         (agent-shell-sidebar-mode)
                         (agent-shell-sidebar-refresh)
                         (setq replacement-timer agent-shell-sidebar--parse-timer)))
                     nil))))
        (reentrancy--dispatch))
      (should reinitialized)
      (should-not (eq old-cache agent-shell-sidebar--parse-cache))
      (should (= 0 (hash-table-count agent-shell-sidebar--parse-cache)))
      (should (= 12 agent-shell-sidebar--pending-count))
      (should (eq replacement-timer agent-shell-sidebar--parse-timer))
      (reentrancy--finish)
      (should (= 12 (hash-table-count agent-shell-sidebar--parse-cache))))))

(ert-deftest reentrancy-owner-kill-during-read-cannot-schedule-another-buffer ()
  (reentrancy--fixture
    (let ((old-cache agent-shell-sidebar--parse-cache)
          (after-insert-file-functions
           (list (lambda (_) (kill-buffer owner) nil))))
      (reentrancy--dispatch)
      (should-not (buffer-live-p owner))
      (should (= 0 (hash-table-count old-cache)))
      (should-not (memq owner agent-shell-sidebar--timer-buffers)))))

(ert-deftest reentrancy-nested-timer-cannot-consume-a-second-head ()
  (reentrancy--fixture
    (let ((generation agent-shell-sidebar--parse-generation) nested)
      (let ((after-insert-file-functions
             (list (lambda (_)
                     (unless nested
                       (setq nested t)
                       (with-current-buffer owner
                         ;; A command in a recursive edit can rearm this owner.
                         (agent-shell-sidebar--reset-idle-timers)
                         (reentrancy--dispatch)))
                     nil))))
        (reentrancy--dispatch))
      (should nested)
      (should (eq generation agent-shell-sidebar--parse-generation))
      (should (= 11 agent-shell-sidebar--pending-count))
      (should (string-suffix-p "0001.md" (car agent-shell-sidebar--parse-queue)))
      (reentrancy--finish)
      (should (= 12 (hash-table-count agent-shell-sidebar--parse-cache))))))

(ert-deftest reentrancy-inner-read-cache-result-survives-outer-read ()
  (reentrancy--fixture
    (let ((head (car agent-shell-sidebar--parse-queue)) nested)
      (let ((after-insert-file-functions
             (list (lambda (_)
                     (unless nested
                       (setq nested t)
                       (reentrancy--write root "0000" "Replacement agent with changed length" 2000000)
                       (with-current-buffer owner
                         (agent-shell-sidebar--ensure-parsed head)))
                     nil))))
        (reentrancy--dispatch))
      (should (equal "Replacement agent with changed length"
                     (plist-get (agent-shell-sidebar--cached-header head) :agent)))
      (reentrancy--finish)
      (should (string-search "Replacement agent with changed length" (buffer-string))))))

(ert-deftest reentrancy-ensure-parsed-still-caches-outside-sidebar-mode ()
  (let* ((root (file-name-as-directory (make-temp-file "sidebar-outside-" t)))
         (file (reentrancy--write root "outside")))
    (unwind-protect
        (with-temp-buffer
          (should-not (derived-mode-p 'agent-shell-sidebar-mode))
          (should (equal "Original" (plist-get (agent-shell-sidebar--ensure-parsed file) :agent)))
          (cl-letf (((symbol-function 'agent-shell-sidebar--parse-header-from-file)
                     (lambda (_) (ert-fail "Unchanged metadata was read twice"))))
            (should (equal "Original" (plist-get (agent-shell-sidebar--ensure-parsed file) :agent)))))
      (delete-directory root t))))
