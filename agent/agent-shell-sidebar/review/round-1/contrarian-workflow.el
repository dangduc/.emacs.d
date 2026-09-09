;;; contrarian-workflow.el --- Independent workflow probes -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'json)
(setq native-comp-enable-subr-trampolines nil)

(defconst workflow-probe--directory
  (file-name-directory (or load-file-name buffer-file-name)))
(defconst workflow-probe--source
  (symbol-file 'agent-shell-sidebar--ensure-parsed 'defun))

(defmacro workflow-probe--sidebar (&rest body)
  (declare (indent 0))
  `(let ((agent-shell-sidebar-refresh-timer nil)
         (agent-shell-sidebar--parse-cache (make-hash-table :test 'equal)))
     (with-temp-buffer
       (agent-shell-sidebar-mode)
       ,@body)))

(ert-deftest workflow-nonregular-fifo-never-blocks-header-parsing ()
  ;; The helper kills its child after three seconds and deletes its temporary FIFO.
  (let* ((result
          (with-temp-buffer
            (should (= 0 (call-process "python3" nil t nil
                                      (expand-file-name "contrarian-workflow-fifo.py"
                                                        workflow-probe--directory)
                                      workflow-probe--source)))
            (json-parse-string (buffer-string) :object-type 'alist
                               :false-object nil)))
         (blocked (alist-get 'timed_out result)))
    (message "FIFO probe: %s" (alist-get 'output result))
    (message "FIFO probe: timeout=%S elapsed=%S" blocked (alist-get 'elapsed_seconds result))
    (should-not blocked)))

(ert-deftest workflow-control-discovery-retains-only-normal-md-fixture ()
  (let* ((root (make-temp-file "sidebar-workflow-files-" t))
         (directory (expand-file-name ".agent-shell/transcripts" root))
         (file (expand-file-name "ordinary.md" directory))
         (agent-shell-sidebar--file-info (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (make-directory directory t)
          (with-temp-file file (insert "**Agent:** Fixture\n---\n"))
          (make-directory (expand-file-name "directory.md" directory))
          (make-symbolic-link file (expand-file-name "symlink.md" directory))
          (should (equal (list file) (agent-shell-sidebar--transcripts-for-root root))))
      (delete-directory root t))))

(ert-deftest workflow-disable-refresh-before-pending-callback ()
  (workflow-probe--sidebar
    (let ((calls 0)
          (owner (current-buffer))
          (agent-shell-sidebar-refresh-timer 30))
      (agent-shell-sidebar--schedule-refresh)
      (let ((pending agent-shell-sidebar--refresh-timer-object))
        (unwind-protect
            (progn
              (setq agent-shell-sidebar-refresh-timer nil)
              ;; Dispatch the already-scheduled timer through Emacs's own handler.
              (cl-letf (((symbol-function 'get-buffer-window)
                         (lambda (&rest _) (selected-window)))
                        ((symbol-function 'agent-shell-sidebar-refresh)
                         (lambda () (should (eq owner (current-buffer)))
                           (cl-incf calls))))
                (timer-event-handler pending))
              (message "Disabled refresh: callback scans=%d, rearmed=%S"
                       calls (timerp agent-shell-sidebar--refresh-timer-object))
              (should-not agent-shell-sidebar--refresh-timer-object)
              (should (= calls 0)))
          (cancel-timer pending))))))

(ert-deftest workflow-control-mru-disabled-preserves-dedicated-window ()
  (save-window-excursion
    (let ((sidebar (generate-new-buffer " *workflow side window*"))
          (target (generate-new-buffer " *workflow target*"))
          (agent-shell-sidebar-open-file-in-most-recently-used-window nil))
      (unwind-protect
          (let ((window (display-buffer-in-side-window sidebar '((side . left)))))
            (set-window-dedicated-p window t)
            (select-window window)
            (agent-shell-sidebar--pop-to target)
            (should (eq (window-buffer window) sidebar))
            (should (eq (window-buffer (selected-window)) target)))
        (kill-buffer target)
        (kill-buffer sidebar)))))

(ert-deftest workflow-control-cancel-deletion-retains-marks ()
  (workflow-probe--sidebar
    (let ((file (make-temp-file "sidebar-workflow-cancel-" nil ".md" "synthetic")))
      (unwind-protect
          (progn
            (puthash file 'delete agent-shell-sidebar--marks)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                      ((symbol-function 'delete-file)
                       (lambda (&rest _) (ert-fail "Deletion ran after cancellation"))))
              (agent-shell-sidebar-execute))
            (should (file-exists-p file))
            (should (eq 'delete (gethash file agent-shell-sidebar--marks))))
        (delete-file file)))))

(ert-deftest workflow-control-cancel-agent-selector-never-starts ()
  (workflow-probe--sidebar
    (let ((inhibit-read-only t))
      (insert (propertize "project\n" 'agent-shell-sidebar-project temporary-file-directory)))
    (goto-char (point-min))
    (cl-letf (((symbol-function 'agent-shell-sidebar--config-for-agent-name)
               (lambda (_) nil))
              ((symbol-function 'agent-shell--auto-preferred-config)
               (lambda () nil))
              ((symbol-function 'agent-shell-select-config)
               (lambda (&rest _) (signal 'quit nil)))
              ((symbol-function 'agent-shell-sidebar--start)
               (lambda (&rest _) (ert-fail "Started after cancellation"))))
      (should (eq 'cancelled
                  (condition-case nil
                      (agent-shell-sidebar-new-session)
                    (quit 'cancelled)))))))

(ert-deftest workflow-control-two-window-points-survive-redraw ()
  (save-window-excursion
    (delete-other-windows)
    (workflow-probe--sidebar
      (let* ((root (file-name-as-directory temporary-file-directory))
             (files (mapcar (lambda (name) (expand-file-name name root))
                            '("synthetic-one.md" "synthetic-two.md" "synthetic-three.md")))
             (first (selected-window))
             (second (split-window-below)))
        (setq agent-shell-sidebar--groups (list (cons root files)))
        (agent-shell-sidebar--redraw)
        (set-window-buffer first (current-buffer))
        (set-window-buffer second (current-buffer))
        (goto-char (text-property-any (point-min) (point-max)
                                     'agent-shell-sidebar-file (car files)))
        (set-window-point second
                          (text-property-any (point-min) (point-max)
                                             'agent-shell-sidebar-file (caddr files)))
        (agent-shell-sidebar--redraw)
        (should (equal (car files) (get-text-property (window-point first)
                                                    'agent-shell-sidebar-file)))
        (should (equal (caddr files) (get-text-property (window-point second)
                                                      'agent-shell-sidebar-file)))))))

(ert-deftest workflow-toggle-works-after-sidebar-buffer-switched-to-normal-window ()
  (save-window-excursion
    (delete-other-windows)
    (workflow-probe--sidebar
      ;; Model an ordinary C-x b into the retained sidebar buffer.
      (set-window-buffer (selected-window) (current-buffer))
      (let ((agent-shell-sidebar-name (buffer-name))
            (agent-shell-sidebar-pop-to-sidebar-on-toggle-open nil)
            (agent-shell-sidebar-extra-project-roots nil)
            (projectile-known-projects nil)
            (error-message nil))
        (cl-letf (((symbol-function 'project-known-project-roots) (lambda () nil))
                  ((symbol-function 'agent-shell-buffers) (lambda () nil))
                  ((symbol-function 'project-current) (lambda (&rest _) nil)))
          (condition-case error
              (agent-shell-sidebar-toggle-sidebar)
            (error (setq error-message (error-message-string error)))))
        (message "Ordinary-window toggle: error=%S side=%S windows=%d"
                 error-message (window-parameter (selected-window) 'window-side)
                 (length (window-list)))
        (should-not error-message)))))

;;; contrarian-workflow.el ends here
