;;; sidebar-tests.el --- Sidebar behavioral checks -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'agent-shell-sidebar)
(require 'agent-shell-mock-agent)
(defconst sidebar-test--directory (file-name-directory (or load-file-name buffer-file-name)))

(defun sidebar-test--write (root name &optional body)
  (let ((file (expand-file-name (concat ".agent-shell/transcripts/" name ".md") root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (or body (format "# Agent Shell Transcript\n\n**Agent:** Claude\n**Started:** 2026-09-08 12:30:00\n**Working Directory:** %s\n**Session ID:** %s\n**Model:** test-model\n\n---\n\n## User (2026-09-08)\n\nPrompt %s\n" root name name))))
    file))

(defmacro sidebar-test--project (&rest body)
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "sidebar-test-" t)))
          (default-directory root)
          (agent-shell-sidebar-extra-project-roots (list root))
          (agent-shell-sidebar-refresh-timer nil)
          (agent-shell-sidebar--parse-cache (make-hash-table :test 'equal))
          (projectile-known-projects nil))
     (unwind-protect
         (cl-letf (((symbol-function 'project-known-project-roots) (lambda () nil))
                   ((symbol-function 'agent-shell-buffers) (lambda () nil)))
           (with-temp-buffer
             (agent-shell-sidebar-mode)
             ,@body))
       (delete-directory root t))))

(defun sidebar-test--drain ()
  (let ((ticks 0))
    (while (and (or agent-shell-sidebar--parse-queue
                    (timerp agent-shell-sidebar--parse-timer))
                (< ticks 200))
      (agent-shell-sidebar--parse-tick)
      (cl-incf ticks))
    (should-not agent-shell-sidebar--parse-queue)
    (should-not agent-shell-sidebar--parse-timer)
    (should-not (bound-and-true-p agent-shell-sidebar--render-iterator))
    (should-not (bound-and-true-p agent-shell-sidebar--render-error))))

(defun sidebar-test--goto-file (file)
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (equal file (get-text-property (point) 'agent-shell-sidebar-file))))
    (forward-line 1))
  (should-not (eobp)))

(ert-deftest sidebar-metadata-is-confined-to-header ()
  :tags '(regression)
  (sidebar-test--project
    (let* ((file (sidebar-test--write root "header"
                  "# Agent Shell Transcript\n\n**Agent:** Claude\n**Session ID:** \n\n---\n\n## User (today)\n\n**Working Directory:** /wrong/\n**Session ID:** wrong\n**Model:** wrong\n"))
           (header (agent-shell-sidebar--parse-header-from-file file)))
      (should (equal (plist-get header :agent) "Claude"))
      (should-not (plist-get header :session-id))
      (should-not (plist-get header :cwd))
      (should-not (plist-get header :model)))))

(ert-deftest sidebar-empty-prompt-does-not-preview-agent-message ()
  :tags '(regression)
  (sidebar-test--project
    (let ((file (sidebar-test--write root "empty"
                  "# Agent Shell Transcript\n\n---\n\n## User (today)\n\n## Agent (today)\n\nresponse\n")))
      (should-not (plist-get (agent-shell-sidebar--parse-header-from-file file) :preview)))))

(ert-deftest sidebar-parser-is-bounded-and-strips-quote ()
  (sidebar-test--project
    (let* ((body (concat "# Agent Shell Transcript\n**Agent:** Claude\n---\n## User (now)\n\n> hello\n"
                         (make-string 10000 ?x) "\n**Session ID:** late\n"))
           (header (agent-shell-sidebar--parse-header-from-file (sidebar-test--write root "large" body))))
      (should (equal (plist-get header :preview) "hello"))
      (should-not (plist-get header :session-id)))))

(ert-deftest sidebar-empty-config-name-never-matches ()
  :tags '(regression)
  (let ((agent-shell-agent-configs '(((:mode-line-name . "Other")))))
    (should-not (agent-shell-sidebar--config-for-agent-name "Claude"))))

(ert-deftest sidebar-ambiguous-config-name-never-selects-first ()
  :tags '(regression)
  (let ((agent-shell-agent-configs '(((:mode-line-name . "Claude One") (:buffer-name . "One"))
                                     ((:mode-line-name . "Claude Two") (:buffer-name . "Two")))))
    (should-not (agent-shell-sidebar--config-for-agent-name "Claude"))))

(ert-deftest sidebar-exact-config-beats-substring ()
  (let* ((expected '((:mode-line-name . "Claude")))
         (agent-shell-agent-configs (list '((:mode-line-name . "Claude Other")) expected)))
    (should (equal expected (agent-shell-sidebar--config-for-agent-name "cLaUdE")))))

(ert-deftest sidebar-unknown-recorded-agent-does-not-use-preferred-agent ()
  (let (prompted)
    (cl-letf (((symbol-function 'agent-shell-sidebar--config-for-agent-name) (lambda (_) nil))
              ((symbol-function 'agent-shell--auto-preferred-config) (lambda () 'wrong))
              ((symbol-function 'agent-shell-select-config) (lambda (&rest _) (setq prompted t) 'chosen)))
      (should (eq (agent-shell-sidebar--select-config "Missing") 'chosen))
      (should prompted))))

(ert-deftest sidebar-draw-and-fold-do-not-rescan ()
  :tags '(regression)
  (sidebar-test--project
    (sidebar-test--write root "one")
    (agent-shell-sidebar-refresh)
    (sidebar-test--drain)
    (cl-letf (((symbol-function 'agent-shell-sidebar--collect) (lambda () (ert-fail "Redraw rescanned projects")))
              ((symbol-function 'file-attributes) (lambda (&rest _) (ert-fail "Redraw read file attributes"))))
      (agent-shell-sidebar--redraw)
      (goto-char (point-min))
      (agent-shell-sidebar-toggle-project))))

(ert-deftest sidebar-idle-chunks-rearm-after-elapsed-idle ()
  :tags '(regression)
  (sidebar-test--project
    (let ((agent-shell-sidebar--parse-queue '("pending"))
          (agent-shell-sidebar-parse-idle-delay 0.1)
          (agent-shell-sidebar--parse-timer nil)
          args)
      (cl-letf (((symbol-function 'current-idle-time) (lambda () (seconds-to-time 30)))
                ((symbol-function 'run-with-idle-timer) (lambda (&rest values) (setq args values) nil)))
        (agent-shell-sidebar--start-parse-timer))
      (should (> (float-time (car args)) 30))
      (should-not (cadr args)))))

(ert-deftest sidebar-chunks-are-bounded-and-preserve-selection ()
  (sidebar-test--project
    (let ((agent-shell-sidebar-parse-chunk-size 2))
      (dotimes (i 5) (sidebar-test--write root (number-to-string i)))
      (agent-shell-sidebar-refresh)
      (should (= (length agent-shell-sidebar--parse-queue) 5))
      (agent-shell-sidebar--parse-tick)
      (should (= (length agent-shell-sidebar--parse-queue) 3))
      (sidebar-test--goto-file (expand-file-name ".agent-shell/transcripts/4.md" root))
      (let ((file (get-text-property (point) 'agent-shell-sidebar-file)))
        (sidebar-test--drain)
        (should (equal file (get-text-property (point) 'agent-shell-sidebar-file)))))))

(ert-deftest sidebar-zero-chunk-size-still-progresses ()
  (sidebar-test--project
    (let ((agent-shell-sidebar-parse-chunk-size 0))
      (sidebar-test--write root "one")
      (agent-shell-sidebar-refresh)
      (sidebar-test--drain))))

(ert-deftest sidebar-input-pending-yields-with-queue-intact ()
  (sidebar-test--project
    (sidebar-test--write root "one")
    (agent-shell-sidebar-refresh)
    (cl-letf (((symbol-function 'input-pending-p) (lambda () t)))
      (agent-shell-sidebar--parse-tick))
    (should (= (length agent-shell-sidebar--parse-queue) 1))
    (should (timerp agent-shell-sidebar--parse-timer))))

(ert-deftest sidebar-timers-belong-to-their-buffer ()
  (sidebar-test--project
    (sidebar-test--write root "one")
    (agent-shell-sidebar-refresh)
    (let ((timer agent-shell-sidebar--parse-timer)
          (owner (current-buffer)))
      (with-temp-buffer
        (agent-shell-sidebar-mode)
        (should-not agent-shell-sidebar--parse-timer))
      (should (memq timer timer-idle-list))
      (with-current-buffer owner (agent-shell-sidebar--cleanup))
      (should-not (memq timer timer-idle-list)))))

(ert-deftest sidebar-kill-and-mode-change-cancel-both-timers ()
  (dolist (action '(kill-buffer fundamental-mode))
    (let ((buffer (generate-new-buffer " *sidebar lifecycle*")) timers)
      (unwind-protect
          (with-current-buffer buffer
            (let ((agent-shell-sidebar-refresh-timer 30)) (agent-shell-sidebar-mode))
            (setq agent-shell-sidebar--parse-queue '("unused"))
            (agent-shell-sidebar--start-parse-timer)
            (setq timers (list agent-shell-sidebar--parse-timer agent-shell-sidebar--refresh-timer-object))
            (funcall action)
            (dolist (timer timers) (should-not (memq timer timer-idle-list))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest sidebar-refresh-scans-once-and-discards-obsolete-queue ()
  (sidebar-test--project
    (let ((file (sidebar-test--write root "gone"))
          (collect (symbol-function 'agent-shell-sidebar--collect))
          (calls 0))
      (cl-letf (((symbol-function 'agent-shell-sidebar--collect)
                 (lambda () (cl-incf calls) (funcall collect))))
        (agent-shell-sidebar-refresh)
        (should (= calls 1))
        (delete-file file)
        (agent-shell-sidebar-refresh)
        (should (= calls 2))
        (should-not agent-shell-sidebar--parse-queue)))))

(ert-deftest sidebar-missing-file-is-not-a-stale-cache-hit ()
  (sidebar-test--project
    (let ((file (sidebar-test--write root "gone")))
      (agent-shell-sidebar-refresh)
      (sidebar-test--drain)
      (delete-file file)
      (should (plist-get (agent-shell-sidebar--ensure-parsed file) :error)))))

(ert-deftest sidebar-cache-detects-size-change-with-identical-mtime ()
  (sidebar-test--project
    (let* ((file (sidebar-test--write root "change"))
           (mtime (file-attribute-modification-time (file-attributes file))))
      (agent-shell-sidebar--ensure-parsed file)
      (with-temp-file file (insert "**Agent:** Changed Name\n---\n"))
      (set-file-times file mtime)
      (should (equal "Changed Name" (plist-get (agent-shell-sidebar--ensure-parsed file) :agent))))))

(ert-deftest sidebar-discovery-skips-directories-and-remote-roots ()
  (sidebar-test--project
    (sidebar-test--write root "one")
    (make-directory (expand-file-name ".agent-shell/transcripts/not-a-file.md" root))
    (let ((agent-shell-sidebar-extra-project-roots (list root root "/ssh:invalid:/work/"))
          (agent-shell-sidebar-include-remote-projects nil))
      (should (equal (agent-shell-sidebar--project-roots) (list root)))
      (should (= 1 (length (cdar (agent-shell-sidebar--collect))))))))

(ert-deftest sidebar-root-labels-are-distinct ()
  (let* ((roots '("/a/same/" "/b/same/" "/same/" "/"))
         (names (agent-shell-sidebar--disambiguated-names roots))
         (labels (mapcar (lambda (root) (gethash root names)) roots)))
    (should (= 4 (length (delete-dups labels))))
    (should (equal (gethash "/" names) "/"))))

(ert-deftest sidebar-filter-preserves-marks-and-folds ()
  (sidebar-test--project
    (let ((one (sidebar-test--write root "one"))
          (two (sidebar-test--write root "two")))
      (agent-shell-sidebar-refresh)
      (sidebar-test--drain)
      (puthash one 'delete agent-shell-sidebar--marks)
      (agent-shell-sidebar-set-filter "PROMPT TWO")
      (should (string-match-p "Prompt two" (buffer-string)))
      (should-not (string-match-p "Prompt one" (buffer-string)))
      (should (eq 'delete (gethash one agent-shell-sidebar--marks)))
      (agent-shell-sidebar-set-filter "no such text")
      (should (string-match-p "no matching transcripts" (buffer-string)))
      (agent-shell-sidebar-set-filter "")
      (sidebar-test--goto-file two)
      (should (string-match-p "Prompt one" (buffer-string))))))

(ert-deftest sidebar-recorded-subdirectory-survives-agent-project-detection ()
  (sidebar-test--project
    (let ((directory (expand-file-name "sub/" root))
          (buffer (generate-new-buffer " *sidebar started*")) args)
      (make-directory directory)
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell--start)
                     (lambda (&rest values)
                       (setq args values)
                       (should (equal directory (agent-shell-cwd)))
                       buffer)))
            (should (eq buffer (agent-shell-sidebar--start directory 'config "session")))
            (should (equal (plist-get args :session-id) "session"))
            (should (plist-get args :new-session))
            (should (plist-get args :no-focus))
            (with-current-buffer buffer (should (equal directory (agent-shell-cwd)))))
        (kill-buffer buffer)))))

(ert-deftest sidebar-working-directory-fallback-is-transcript-project ()
  (sidebar-test--project
    (let ((default-directory "/")
          (file (sidebar-test--write root "one")))
      (should (equal root (agent-shell-sidebar--working-directory file nil)))
      (should-error (agent-shell-sidebar--working-directory file '(:cwd "missing/")) :type 'user-error))))

(ert-deftest sidebar-live-session-reuse-is-scoped-and-includes-connecting ()
  (let ((one (generate-new-buffer " *sidebar agent one*"))
        (two (generate-new-buffer " *sidebar agent two*")))
    (unwind-protect
        (progn
          (with-current-buffer one
            (setq-local default-directory "/tmp/")
            (setq-local agent-shell--state '((:agent-config . ((:mode-line-name . "Other"))) (:session . ((:id . "same"))))))
          (with-current-buffer two
            (setq-local default-directory "/tmp/")
            (setq-local agent-shell--state '((:agent-config . ((:mode-line-name . "Claude"))) (:resume-session-id . "same"))))
          (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list one two))))
            (should (eq two (agent-shell-sidebar--live-buffer-for-session "same" "Claude" "/tmp/")))
            (should-not (agent-shell-sidebar--live-buffer-for-session "same" "Claude" "/elsewhere/"))))
      (kill-buffer one) (kill-buffer two))))

(ert-deftest sidebar-visit-at-eol-starts-with-transcript-session ()
  (sidebar-test--project
    (let ((file (sidebar-test--write root "session")) started shown)
      (agent-shell-sidebar-refresh)
      (sidebar-test--drain)
      (sidebar-test--goto-file file)
      (end-of-line)
      (cl-letf (((symbol-function 'agent-shell-sidebar--select-config) (lambda (_) 'config))
                ((symbol-function 'agent-shell-sidebar--start) (lambda (&rest args) (setq started args) 'buffer))
                ((symbol-function 'agent-shell-sidebar--pop-to) (lambda (buffer) (setq shown buffer))))
        (agent-shell-sidebar-visit))
      (should (equal started (list root 'config "session")))
      (should (eq shown 'buffer)))))

(ert-deftest sidebar-open-transcript-really-uses-view-mode ()
  :tags '(regression)
  (sidebar-test--project
    (let ((file (sidebar-test--write root "one")) opened)
      (unwind-protect
          (progn
            (agent-shell-sidebar-refresh)
            (sidebar-test--goto-file file)
            (cl-letf (((symbol-function 'agent-shell-sidebar--pop-to) (lambda (buffer) (setq opened buffer))))
              (agent-shell-sidebar-open-transcript))
            (should (buffer-local-value 'view-mode opened)))
        (when (buffer-live-p opened) (kill-buffer opened))))))

(ert-deftest sidebar-pop-to-preserves-dedicated-sidebar ()
  (save-window-excursion
    (let ((sidebar (generate-new-buffer " *sidebar window test*"))
          (target (generate-new-buffer " *sidebar target*")))
      (unwind-protect
          (let ((window (display-buffer-in-side-window sidebar '((side . left)))))
            (set-window-dedicated-p window t)
            (select-window window)
            (agent-shell-sidebar--pop-to target)
            (should (eq (window-buffer window) sidebar))
            (should (eq (window-buffer (selected-window)) target)))
        (kill-buffer target) (kill-buffer sidebar)))))

(ert-deftest sidebar-next-line-stays-on-final-row ()
  (sidebar-test--project
    (let ((file (sidebar-test--write root "one")))
      (agent-shell-sidebar-refresh)
      (sidebar-test--goto-file file)
      (agent-shell-sidebar-next-line 10)
      (should (equal file (get-text-property (point) 'agent-shell-sidebar-file))))))

(ert-deftest sidebar-redraw-preserves-scroll-position ()
  (sidebar-test--project
    (dotimes (i 50) (sidebar-test--write root (format "%02d" i)))
    (agent-shell-sidebar-refresh)
    (sidebar-test--drain)
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (goto-char (point-min))
      (forward-line 15)
      (set-window-start (selected-window) (point) t)
      (let ((top (get-text-property (point) 'agent-shell-sidebar-file)))
        (forward-line 4)
        (agent-shell-sidebar--redraw)
        (should (equal top (get-text-property (window-start) 'agent-shell-sidebar-file)))))))

(ert-deftest sidebar-many-transcripts-use-one-scan-and-bounded-chunks ()
  (sidebar-test--project
    (let ((collect (symbol-function 'agent-shell-sidebar--collect))
          (parse (symbol-function 'agent-shell-sidebar--parse-header-from-file))
          (agent-shell-sidebar-parse-chunk-size 30)
          (scans 0) (reads 0) (parse-ticks 0) ticks refresh-ms chunk-ms)
      (dotimes (i 300) (sidebar-test--write root (format "%03d" i)))
      (cl-letf (((symbol-function 'agent-shell-sidebar--collect)
                 (lambda () (cl-incf scans) (funcall collect)))
                ((symbol-function 'agent-shell-sidebar--parse-header-from-file)
                 (lambda (file) (cl-incf reads) (funcall parse file))))
        (let ((start (float-time)))
          (agent-shell-sidebar-refresh)
          (setq refresh-ms (* 1000 (- (float-time) start))))
        (should (= reads 0))
        (while (or agent-shell-sidebar--parse-queue
                   (timerp agent-shell-sidebar--parse-timer))
          (let ((before reads) (start (float-time)))
            (when agent-shell-sidebar--parse-queue (cl-incf parse-ticks))
            (agent-shell-sidebar--parse-tick)
            (push (* 1000 (- (float-time) start)) chunk-ms)
            (should (<= (- reads before) 30))
            (push reads ticks))))
      (should (= scans 1))
      (should (= reads 300))
      (should (= parse-ticks 10))
      (should-not (bound-and-true-p agent-shell-sidebar--render-error))
      (should (string-match-p "Prompt 299" (buffer-string)))
      (message "300 transcripts: refresh %.1f ms, max callback %.1f ms, %d scan, %d reads, %d parse chunks, %d total callbacks"
               refresh-ms (apply #'max chunk-ms) scans reads parse-ticks (length ticks)))))

(ert-deftest sidebar-delete-keeps-failed-and-live-transcript-marks ()
  (sidebar-test--project
    (let ((good (sidebar-test--write root "good"))
          (bad (sidebar-test--write root "bad"))
          (live (sidebar-test--write root "live"))
          (owner (generate-new-buffer " *sidebar owner*"))
          (delete-by-moving-to-trash nil)
          (original-delete (symbol-function 'delete-file)))
      (unwind-protect
          (progn
            (with-current-buffer owner (setq-local agent-shell--transcript-file live))
            (dolist (file (list good bad live)) (puthash file 'delete agent-shell-sidebar--marks))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                      ((symbol-function 'agent-shell-buffers) (lambda () (list owner)))
                      ((symbol-function 'delete-file) (lambda (file &optional trash)
                                                      (if (equal file bad) (signal 'file-error '("denied"))
                                                        (funcall original-delete file trash)))))
              (agent-shell-sidebar-execute))
            (should-not (file-exists-p good))
            (should (file-exists-p bad))
            (should (file-exists-p live))
            (should (= 2 (hash-table-count agent-shell-sidebar--marks))))
        (kill-buffer owner)))))

(ert-deftest sidebar-jump-selects-window-even-when-toggle-does-not ()
  (sidebar-test--project
    (save-window-excursion
      (let ((agent-shell-sidebar-name (buffer-name))
            (agent-shell-sidebar-pop-to-sidebar-on-toggle-open nil))
        (agent-shell-sidebar-jump-to-sidebar)
        (should (eq (selected-window) (agent-shell-sidebar--sidebar-buffer)))))))

(ert-deftest sidebar-real-acp-start-resume-and-reuse ()
  (let* ((root (file-name-as-directory (make-temp-file "sidebar-acp-" t)))
         (directory (expand-file-name "sub/" root))
         (log (expand-file-name "requests.jsonl" root))
         (agent-shell-mock-agent-acp-command
          (list (executable-find "python3")
                (expand-file-name "acp-fixture.py" sidebar-test--directory) log))
         (config (agent-shell-mock-agent-make-agent-config))
         (agent-shell-agent-configs (list config))
         (agent-shell-prefer-viewport-interaction nil)
         (agent-shell-show-welcome-message nil)
         (agent-shell-show-config-icons nil)
         (agent-shell-header-style 'text)
         (agent-shell-inhibit-system-sleep nil)
         (agent-shell-session-restore-verbosity 'full)
         (enable-local-variables nil)
         (enable-local-eval nil)
         buffers)
    (unwind-protect
        (progn
          (make-directory directory)
          ;; A real project root must not override the recorded subdirectory.
          (make-directory (expand-file-name ".git" root))
          (dolist (session '("existing-session" nil))
            (let* ((buffer (agent-shell-sidebar--start directory config session))
                   (expected (or session "fixture-new-session"))
                   (deadline (+ (float-time) 8)))
              (push buffer buffers)
              (while (and (< (float-time) deadline)
                          (not (equal expected
                                      (with-current-buffer buffer
                                        (map-nested-elt agent-shell--state '(:session :id))))))
                (accept-process-output nil 0.02))
              (with-current-buffer buffer
                (should (equal expected (map-nested-elt agent-shell--state '(:session :id)))))))
          (let* ((requests (with-temp-buffer
                             (insert-file-contents log)
                             (mapcar (lambda (line) (json-parse-string line :object-type 'alist))
                                     (split-string (buffer-string) "\n" t))))
                 (load-request (seq-find (lambda (request) (equal (map-elt request 'method) "session/load")) requests))
                 (new-request (seq-find (lambda (request) (equal (map-elt request 'method) "session/new")) requests)))
            (should load-request)
            (should new-request)
            (should (equal directory (file-name-as-directory (map-nested-elt load-request '(params cwd)))))
            (should (equal directory (file-name-as-directory (map-nested-elt new-request '(params cwd)))))
            (should (equal "existing-session" (map-nested-elt load-request '(params sessionId)))))
          ;; Activate a real transcript row.  Starting another process is a failure.
          (with-temp-buffer
            (let ((agent-shell-sidebar-refresh-timer nil)) (agent-shell-sidebar-mode))
            (let ((file (sidebar-test--write root "existing"
                          (format "**Agent:** Mock\n**Session ID:** existing-session\n**Working Directory:** %s\n---\n" directory)))
                  shown)
              (let ((inhibit-read-only t))
                (insert (propertize "existing\n" 'agent-shell-sidebar-file file)))
              (goto-char (point-min))
              (cl-letf (((symbol-function 'agent-shell-sidebar--start) (lambda (&rest _) (ert-fail "Started duplicate session")))
                        ((symbol-function 'agent-shell-sidebar--pop-to) (lambda (buffer) (setq shown buffer))))
                (agent-shell-sidebar-visit))
              (should (eq shown (cadr buffers)))))
          (message "ACP fixture: session/load + session/new used recorded subdirectory; RET reused existing shell"))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local kill-buffer-query-functions nil)
            (kill-buffer buffer))))
      (delete-directory root t))))

(ert-deftest sidebar-refuses-to-overwrite-non-sidebar-buffer ()
  (with-temp-buffer
    (insert "keep this content")
    (let ((agent-shell-sidebar-name (buffer-name)))
      (should-error (agent-shell-sidebar--get-or-create-buffer) :type 'user-error)
      (should (equal (buffer-string) "keep this content")))))

;;; sidebar-tests.el ends here
