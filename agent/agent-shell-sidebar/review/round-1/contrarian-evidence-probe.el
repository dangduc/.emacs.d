;;; contrarian-evidence-probe.el --- Independent parser probes -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'agent-shell-sidebar)

(defmacro contrarian-evidence--fixture (&rest body)
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "sidebar-evidence-" t)))
          (file (expand-file-name ".agent-shell/transcripts/fixture.md" root))
          (default-directory root)
          (agent-shell-sidebar-refresh-timer nil)
          (agent-shell-sidebar--parse-cache (make-hash-table :test 'equal)))
     (unwind-protect
         (progn
           (make-directory (file-name-directory file) t)
           ,@body)
       (delete-directory root t))))

(ert-deftest contrarian-evidence-first-message-is-a-boundary-without-separator ()
  (contrarian-evidence--fixture
    (with-temp-file file
      (insert "# Agent Shell Transcript\n**Agent:** Mock\n## User (today)\n"
              "**Session ID:** prompt-text\n**Working Directory:** /wrong/\n"))
    (let ((header (agent-shell-sidebar--parse-header-from-file file)))
      (should (equal (plist-get header :agent) "Mock"))
      (should-not (plist-get header :session-id))
      (should-not (plist-get header :cwd)))))

(ert-deftest contrarian-evidence-byte-limit-is-independent-of-boundary ()
  (contrarian-evidence--fixture
    (with-temp-file file
      (insert "# Agent Shell Transcript\n" (make-string 9000 ?x)
              "\n**Agent:** Too Late\n---\n"))
    (let ((header (agent-shell-sidebar--parse-header-from-file file)))
      (should-not (plist-get header :agent)))))

(ert-deftest contrarian-evidence-session-id-cut-by-byte-limit-is-not-used ()
  (contrarian-evidence--fixture
    (let* ((prefix (format "# Agent Shell Transcript\n**Agent:** Mock\n**Working Directory:** %s\n" root))
           (label "\n**Session ID:** ")
           (session-id "complete-session-identifier")
           ;; Exactly four bytes of the identifier fit in the read window.
           (padding (- 8192 (string-bytes prefix) (string-bytes label) 4)))
      (with-temp-file file
        (insert prefix (make-string padding ?x) label session-id "\n---\n"))
      (let ((header (agent-shell-sidebar--parse-header-from-file file)) started visit-error)
        (with-temp-buffer
          (agent-shell-sidebar-mode)
          (let ((inhibit-read-only t))
            (insert (propertize "fixture\n" 'agent-shell-sidebar-file file)))
          (goto-char (point-min))
          (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () nil))
                    ((symbol-function 'agent-shell-sidebar--select-config) (lambda (_) 'mock))
                    ((symbol-function 'agent-shell-sidebar--start)
                     (lambda (&rest args) (setq started args) 'mock-buffer))
                    ((symbol-function 'agent-shell-sidebar--pop-to) #'ignore))
            (condition-case err
                (agent-shell-sidebar-visit)
              (user-error (setq visit-error err)))))
        (message "Cutoff: complete=%S parsed=%S start-session=%S file-bytes=%d visit-error=%S"
                 session-id (plist-get header :session-id) (nth 2 started)
                 (file-attribute-size (file-attributes file)) visit-error)
        (should (or (plist-get header :error)
                    (not (plist-get header :session-id))))
        (should-not (equal "comp" (nth 2 started)))))))

(ert-deftest contrarian-evidence-real-writer-preserves-directory-ending-in-space ()
  (contrarian-evidence--fixture
    (let* ((directory (expand-file-name "project " root))
           (trimmed (expand-file-name "project" root))
           (agent-shell-cwd-function (lambda () directory)))
      (make-directory directory)
      ;; Both names exist, so silently choosing the wrong one is observable.
      (make-directory trimmed)
      (with-temp-buffer
        ;; Exercise the real package's writer without starting an agent.
        (setq major-mode 'agent-shell-mode)
        (setq-local agent-shell--transcript-file file)
        (setq-local agent-shell--state
                    '((:agent-config . ((:mode-line-name . "Mock")))
                      (:session . ((:id . "existing-session")))))
        (should (agent-shell--ensure-transcript-file)))
      (let* ((header (agent-shell-sidebar--parse-header-from-file file))
             (resolved (agent-shell-sidebar--working-directory file header)))
        (message "Real writer: recorded=%S parsed=%S resolved=%S"
                 directory (plist-get header :cwd) resolved)
        (should (equal resolved (file-name-as-directory directory)))))))

(ert-deftest contrarian-evidence-complete-id-at-byte-limit-is-retained ()
  (contrarian-evidence--fixture
    (let* ((prefix "# Agent Shell Transcript\n")
           (tail "\n**Session ID:** complete\n")
           (padding (- 8192 (string-bytes prefix) (string-bytes tail))))
      (with-temp-file file
        (insert prefix (make-string padding ?x) tail "---\n"))
      (should (equal "complete"
                     (plist-get (agent-shell-sidebar--parse-header-from-file file) :session-id))))))

;;; contrarian-evidence-probe.el ends here
