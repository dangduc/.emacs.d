;;; viewing-tests.el --- Transcript viewing regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'agent-shell-sidebar)

(defmacro viewing--fixture (&rest body)
  "Run BODY on a real discovered transcript row in a temporary project."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "sidebar-viewing-" t)))
          (file (expand-file-name ".agent-shell/transcripts/read.md" root))
          (default-directory root)
          (agent-shell-sidebar-extra-project-roots (list root))
          (agent-shell-sidebar-refresh-timer nil)
          (projectile-known-projects nil)
          (enable-local-variables nil)
          (enable-local-eval nil)
          visited)
     (unwind-protect
         (cl-letf (((symbol-function 'project-known-project-roots) (lambda () nil))
                   ((symbol-function 'agent-shell-buffers) (lambda () nil))
                   ((symbol-function 'agent-shell-sidebar--pop-to) (lambda (buffer) (setq visited buffer))))
           (make-directory (file-name-directory file) t)
           (with-temp-file file (insert "**Agent:** Fixture\n---\n## User\nA real transcript\n"))
           (with-temp-buffer
             (agent-shell-sidebar-mode)
             (agent-shell-sidebar-refresh)
             (goto-char (point-min))
             (let ((match (text-property-search-forward 'agent-shell-sidebar-file file #'equal)))
               (should match)
               (goto-char (prop-match-beginning match)))
             ,@body))
       (when (buffer-live-p visited) (kill-buffer visited))
       (delete-directory root t))))

(ert-deftest viewing-regular-transcript-retains-real-view-mode ()
  (viewing--fixture
    (agent-shell-sidebar-open-transcript)
    (should (buffer-live-p visited))
    (should (equal file (buffer-local-value 'buffer-file-name visited)))
    (should (buffer-local-value 'view-mode visited))
    (should (buffer-local-value 'buffer-read-only visited))
    (with-current-buffer visited
      (should (string-search "A real transcript" (buffer-string))))))

(ert-deftest viewing-missing-and-unreadable-files-retain-readable-error ()
  (dolist (kind '(missing unreadable))
    (viewing--fixture
      (if (eq kind 'missing) (delete-file file) (set-file-modes file #o000))
      (unwind-protect
          (cl-letf (((symbol-function 'find-file-noselect)
                     (lambda (&rest _) (ert-fail "Attempted to open an unreadable path"))))
            (let ((err (should-error (agent-shell-sidebar-open-transcript) :type 'user-error)))
              (should (string-prefix-p "Cannot read transcript:" (error-message-string err))))
            (should-not visited))
        (when (eq kind 'unreadable) (set-file-modes file #o600))))))

(ert-deftest viewing-rejects-nonregular-replacements-before-file-open ()
  (dolist (kind '(fifo directory symlink))
    (viewing--fixture
      (delete-file file)
      (pcase kind
        ('fifo (should (zerop (call-process "mkfifo" nil nil nil file))))
        ('directory (make-directory file))
        ('symlink
         (let ((target (expand-file-name "target.md" root)))
           (with-temp-file target (insert "Regular target\n"))
           (make-symbolic-link target file))))
      (should (file-readable-p file))
      (cl-letf (((symbol-function 'find-file-noselect)
                 (lambda (&rest _) (ert-fail "Nonregular path reached file open"))))
        (let ((err (should-error (agent-shell-sidebar-open-transcript) :type 'user-error)))
          (should (string-prefix-p "Not a regular transcript file:" (error-message-string err))))
        (should-not visited)))))

;;; viewing-tests.el ends here
