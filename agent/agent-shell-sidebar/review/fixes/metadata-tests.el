;;; metadata-tests.el --- Parser and cache regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'agent-shell-sidebar)

(defmacro metadata-test--fixture (&rest body)
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "sidebar-metadata-" t)))
          (file (expand-file-name ".agent-shell/transcripts/fixture.md" root))
          (default-directory root)
          (agent-shell-sidebar-refresh-timer nil)
          (agent-shell-sidebar-extra-project-roots (list root))
          (projectile-known-projects nil))
     (unwind-protect
         (cl-letf (((symbol-function 'project-known-project-roots) (lambda () nil))
                   ((symbol-function 'agent-shell-buffers) (lambda () nil)))
           (make-directory (file-name-directory file) t)
           ,@body)
       (when (file-exists-p file) (set-file-modes file #o600))
       (delete-directory root t))))

(defun metadata-test--write (file root)
  (with-temp-file file
    (insert (format "**Agent:** Mock\n**Session ID:** complete-session\n**Working Directory:** %s\n---\n## User\nFirst prompt\n" root))))

(defun metadata-test--visit (file)
  "Activate FILE using the real command, with startup forbidden."
  (with-temp-buffer
    (agent-shell-sidebar-mode)
    (let ((inhibit-read-only t))
      (insert (propertize "fixture\n" 'agent-shell-sidebar-file file)))
    (goto-char (point-min))
    (cl-letf (((symbol-function 'agent-shell-sidebar--start)
               (lambda (&rest _) (ert-fail "Activation started an incomplete transcript")))
              ((symbol-function 'agent-shell-sidebar--select-config)
               (lambda (&rest _) (ert-fail "Activation requested configuration for an incomplete transcript"))))
      (should-error (agent-shell-sidebar-visit) :type 'user-error))))

(ert-deftest metadata-truncated-activation-fields-never-start-a-session ()
  (metadata-test--fixture
    (let ((full-directory (expand-file-name "project " root))
          (wrong-directory (expand-file-name "project" root)))
      (make-directory full-directory)
      (make-directory wrong-directory)
      (dolist (field `(("Session ID" :session-id "complete-session")
                       ("Working Directory" :cwd ,full-directory)
                       ("Agent" :agent "Complete Agent")))
        (let* ((prefix (concat "# Agent Shell Transcript\n"
                               (unless (eq (nth 1 field) :agent) "**Agent:** Mock\n")
                               (unless (eq (nth 1 field) :cwd)
                                 (format "**Working Directory:** %s\n" root))))
               (label (format "\n**%s:** " (car field)))
               (value (nth 2 field))
               (padding (- 8192 (string-bytes prefix) (string-bytes label)
                           (1- (string-bytes value)))))
          (with-temp-file file
            (insert prefix (make-string padding ?x) label value "\n---\n"))
          (let ((header (agent-shell-sidebar--parse-header-from-file file)))
            (should (string-match-p "8192-byte read limit" (plist-get header :error)))
            (should-not (plist-get header (nth 1 field))))
          (metadata-test--visit file))))))

(ert-deftest metadata-byte-limit-retains-only-complete-lines ()
  (metadata-test--fixture
    (dolist (tail '("\n**Session ID:** complete\n" "\n**Session ID:** complete"))
      (with-temp-file file
        (insert (make-string (- 8192 (string-bytes tail)) ?x) tail "-continuation\n---\n"))
      (let ((header (agent-shell-sidebar--parse-header-from-file file)))
        (should (plist-get header :error))
        (if (string-suffix-p "\n" tail)
            (should (equal "complete" (plist-get header :session-id)))
          (should-not (plist-get header :session-id)))))))

(ert-deftest metadata-true-eof-retains-an-unterminated-complete-field ()
  (metadata-test--fixture
    (with-temp-file file (insert "**Session ID:** complete"))
    (let ((header (agent-shell-sidebar--parse-header-from-file file)))
      (should-not (plist-get header :error))
      (should (equal "complete" (plist-get header :session-id))))))

(ert-deftest metadata-growth-during-read-does-not-complete-a-cutoff-field ()
  (metadata-test--fixture
    (let* ((label "\n**Session ID:** ")
           (padding (- 8192 (string-bytes label) 4))
           (read-file (symbol-function 'insert-file-contents)))
      (with-temp-file file (insert (make-string padding ?x)))
      (cl-letf (((symbol-function 'insert-file-contents)
                 (lambda (&rest args)
                   ;; Append after the parser checked the old, shorter size.
                   (write-region (concat label "complete-session\n---\n") nil file t 'silent)
                   (apply read-file args))))
        (let ((header (agent-shell-sidebar--parse-header-from-file file)))
          (should (plist-get header :error))
          (should-not (plist-get header :session-id)))))))

(ert-deftest metadata-bounded-preview-does-not-invalidate-complete-header ()
  (metadata-test--fixture
    (metadata-test--write file root)
    (write-region (make-string 10000 ?x) nil file t 'silent)
    (let ((header (agent-shell-sidebar--parse-header-from-file file)))
      (should-not (plist-get header :error))
      (should (equal "complete-session" (plist-get header :session-id)))
      (should (equal "First prompt" (plist-get header :preview))))))

(ert-deftest metadata-real-writer-preserves-directory-whitespace ()
  (metadata-test--fixture
    (dolist (relative '("project " "project\t" " leading" " "))
      (let* ((directory (expand-file-name relative root))
             (agent-shell-cwd-function (lambda () directory)))
        (make-directory directory)
        (when (file-exists-p file) (delete-file file))
        (with-temp-buffer
          (setq major-mode 'agent-shell-mode)
          (setq-local agent-shell--transcript-file file)
          (setq-local agent-shell--state
                      '((:agent-config . ((:mode-line-name . "Mock")))
                        (:session . ((:id . "complete-session")))))
          (should (agent-shell--ensure-transcript-file)))
        (let ((header (agent-shell-sidebar--parse-header-from-file file)))
          (should (equal directory (plist-get header :cwd)))
          (should (equal (file-name-as-directory directory)
                         (agent-shell-sidebar--working-directory file header))))))))

(ert-deftest metadata-empty-directory-is-distinct-from-whitespace-directory ()
  (metadata-test--fixture
    (make-directory (expand-file-name " " root))
    (dolist (record '("**Working Directory:** \n---\n"
                      "**Working Directory:**  \n---\n"))
      (with-temp-file file (insert record))
      (let ((header (agent-shell-sidebar--parse-header-from-file file)))
        (should (equal (plist-get header :cwd)
                       (if (string-prefix-p "**Working Directory:**  " record) " " nil)))))))

(ert-deftest metadata-permission-errors-retry-on-refresh-and-activation ()
  (metadata-test--fixture
    (metadata-test--write file root)
    (with-temp-buffer
      (agent-shell-sidebar-mode)
      (agent-shell-sidebar-refresh)
      (let ((signature (agent-shell-sidebar--signature (file-attributes file))))
        (set-file-modes file #o000)
        (should-not (file-readable-p file))
        (should (plist-get (agent-shell-sidebar--ensure-parsed file) :error))
        (set-file-modes file #o600)
        (should (equal signature (agent-shell-sidebar--signature (file-attributes file))))
        (agent-shell-sidebar-refresh)
        (should (equal (list file) agent-shell-sidebar--parse-queue))
        (should (equal "complete-session"
                       (plist-get (agent-shell-sidebar--ensure-parsed file) :session-id)))
        ;; RET must retry the same cached failure without a refresh first.
        (set-file-modes file #o000)
        (clrhash agent-shell-sidebar--parse-cache)
        (should (plist-get (agent-shell-sidebar--ensure-parsed file) :error))
        (set-file-modes file #o600)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "fixture\n" 'agent-shell-sidebar-file file)))
        (goto-char (point-min))
        (let (started)
          (cl-letf (((symbol-function 'agent-shell-sidebar--select-config) (lambda (_) 'mock))
                    ((symbol-function 'agent-shell-sidebar--start)
                     (lambda (directory config session-id)
                       (setq started (list directory config session-id))
                       (current-buffer)))
                    ((symbol-function 'agent-shell-sidebar--pop-to) #'ignore))
            (agent-shell-sidebar-visit))
          (should (equal (list root 'mock "complete-session") started)))))))

(ert-deftest metadata-cache-belongs-to-each-sidebar-snapshot ()
  (metadata-test--fixture
    (metadata-test--write file root)
    (let ((one (generate-new-buffer " *metadata snapshot one*"))
          (two (generate-new-buffer " *metadata snapshot two*")))
      (unwind-protect
          (progn
            (with-current-buffer one
              (agent-shell-sidebar-mode)
              (agent-shell-sidebar-refresh)
              (agent-shell-sidebar--ensure-parsed file))
            (write-region "\n## Agent\nNew response\n" nil file t 'silent)
            (with-current-buffer two
              (agent-shell-sidebar-mode)
              (agent-shell-sidebar-refresh)
              (agent-shell-sidebar--ensure-parsed file))
            (should-not (eq (buffer-local-value 'agent-shell-sidebar--parse-cache one)
                            (buffer-local-value 'agent-shell-sidebar--parse-cache two)))
            (with-current-buffer one
              (cl-letf (((symbol-function 'file-attributes)
                         (lambda (&rest _) (ert-fail "Snapshot redraw accessed the disk")))
                        ((symbol-function 'insert-file-contents)
                         (lambda (&rest _) (ert-fail "Snapshot redraw read a transcript"))))
                (agent-shell-sidebar--redraw))
              (should (string-match-p "First prompt" (buffer-string)))))
        (kill-buffer one)
        (kill-buffer two)))))

(ert-deftest metadata-discovery-and-parser-reject-special-files ()
  (metadata-test--fixture
    (should (zerop (call-process "mkfifo" nil nil nil file)))
    (let ((agent-shell-sidebar--file-info (make-hash-table :test 'equal)))
      ;; POSIX reports nil type for both regular files and FIFOs.
      (should-not (file-attribute-type (file-attributes file)))
      (should-not (file-regular-p file))
      (should-not (agent-shell-sidebar--transcripts-for-root root))
      (should (plist-get (agent-shell-sidebar--parse-header-from-file file) :error))
      (should (plist-get (agent-shell-sidebar--ensure-parsed file) :error)))))

(ert-deftest metadata-rechecks-a-discovered-file-before-reading ()
  (metadata-test--fixture
    (metadata-test--write file root)
    (with-temp-buffer
      (agent-shell-sidebar-mode)
      (agent-shell-sidebar-refresh)
      (should (equal (list file) agent-shell-sidebar--parse-queue))
      (delete-file file)
      (should (zerop (call-process "mkfifo" nil nil nil file)))
      (should (plist-get (agent-shell-sidebar--ensure-parsed file) :error))
      (metadata-test--visit file))))

(ert-deftest metadata-parser-rechecks-after-activation-attributes-were-read ()
  (metadata-test--fixture
    (metadata-test--write file root)
    (let ((parse (symbol-function 'agent-shell-sidebar--parse-header-from-file)))
      (cl-letf (((symbol-function 'agent-shell-sidebar--parse-header-from-file)
                 (lambda (path)
                   (delete-file path)
                   (should (zerop (call-process "mkfifo" nil nil nil path)))
                   (funcall parse path))))
        (should (plist-get (agent-shell-sidebar--ensure-parsed file) :error))))))

;;; metadata-tests.el ends here
