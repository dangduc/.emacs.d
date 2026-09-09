;;; contrarian-evidence-tests.el --- Schema and cache boundary oracles -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'json)
(defconst ce2--uuid "33333333-3333-7333-8333-333333333333")
(defun ce2--codex-meta ()
  `((type . "session_meta") (payload . ((id . ,ce2--uuid) (cwd . ,temporary-file-directory)))))
(defun ce2--codex-user (content)
  `((type . "response_item") (payload . ((type . "message") (role . "user") (content . ,content)))))
(defun ce2--claude-user (content)
  `((type . "user") (cwd . ,temporary-file-directory)
    (message . ((role . "user") (content . ,content)))))
(defun ce2--write (file records)
  (with-temp-file file
    (dolist (record records) (insert (json-encode record) "\n"))))
(defmacro ce2--file (&rest body)
  (declare (indent 0))
  `(let ((file (make-temp-file "ce2-schema-" nil ".jsonl")))
     (unwind-protect (progn ,@body) (delete-file file))))

(ert-deftest ce2-unexpected-codex-content-shape-does-not-poison-valid-session ()
  (ce2--file
    (ce2--write file
                (list (ce2--codex-meta)
                      (ce2--codex-user '((type . "input_text") (text . "INVALID OBJECT SHAPE")))
                      (ce2--codex-user [((type . "input_text") (text . "VALID REQUEST"))])))
    (let ((meta (agent-sidebar--codex-parse (list :id file))))
      (should-not (plist-get meta :error))
      (should (equal (plist-get meta :session-id) ce2--uuid))
      (should (equal (plist-get meta :preview) "VALID REQUEST")))))

(ert-deftest ce2-unexpected-claude-content-shape-does-not-poison-valid-session ()
  (ce2--file
    (ce2--write file
                (list (ce2--claude-user '((type . "text") (text . "INVALID OBJECT SHAPE")))
                      (ce2--claude-user [((type . "text") (text . "VALID REQUEST"))])))
    (let ((meta (agent-sidebar--claude-parse (list :id file :extras (list :session-id ce2--uuid)))))
      (should-not (plist-get meta :error))
      (should (equal (plist-get meta :session-id) ce2--uuid))
      (should (equal (plist-get meta :preview) "VALID REQUEST")))))

(ert-deftest ce2-completing-a-native-record-invalidates-a-successful-empty-preview-cache ()
  (ce2--file
    (ce2--write file (list (ce2--codex-meta)))
    (let* ((record (json-encode (ce2--codex-user [((type . "input_text") (text . "APPENDED REQUEST"))])))
           (prefix (substring record 0 -4)) (suffix (substring record -4)))
      (with-temp-buffer (insert prefix) (write-region (point-min) (point-max) file t 'silent))
      (with-temp-buffer
        (agent-sidebar-mode)
    (setq-local agent-shell-sidebar--file-info (make-hash-table :test 'equal))
        (let ((entry (list :id file :provider 'codex-cli)))
          (puthash file entry agent-sidebar--entries)
          (let ((first (agent-sidebar--ensure-parsed entry)))
            (should (equal (plist-get first :session-id) ce2--uuid))
            (should-not (plist-get first :preview))
            (cl-letf (((symbol-function 'agent-sidebar--codex-parse)
                       (lambda (_) (ert-fail "Unchanged file was unexpectedly reparsed"))))
              (should (eq first (agent-sidebar--ensure-parsed entry)))))
          (with-temp-buffer (insert suffix "\n") (write-region (point-min) (point-max) file t 'silent))
          (let ((second (agent-sidebar--ensure-parsed entry)))
            (should (equal (plist-get second :session-id) ce2--uuid))
            (should (equal (plist-get second :preview) "APPENDED REQUEST"))))))))

(ert-deftest ce2-multibyte-record-crossing-byte-prefix-does-not-leak-partial-preview ()
  (ce2--file
    (ce2--write file (list (ce2--codex-meta)
                           (ce2--codex-user (vector `((type . "input_text") (text . ,(make-string 100000 ?界)))))))
    (should (> (file-attribute-size (file-attributes file)) 262144))
    (let ((meta (agent-sidebar--codex-parse (list :id file))))
      (should (equal (plist-get meta :session-id) ce2--uuid))
      (should-not (plist-get meta :preview))
      (should-not (plist-get meta :error)))))

(defun ce2--check-cache-preview-bound (file)
  (with-temp-buffer
    (agent-sidebar-mode)
    (setq-local agent-shell-sidebar--file-info (make-hash-table :test 'equal))
    (let ((entry (list :id file :provider 'codex-cli)))
      (puthash file entry agent-sidebar--entries)
      (let ((meta (agent-sidebar--ensure-parsed entry)))
        (should (= (length (plist-get meta :preview)) 200))
        (should (string-suffix-p "…" (plist-get meta :preview)))
        (should (equal (plist-get meta :session-id) ce2--uuid))
        (should (eq meta (agent-sidebar--cached-meta entry)))))))

(ert-deftest ce2-preview-bound-survives-cache-and-activation-path ()
  (ce2--file
    (ce2--write file (list (ce2--codex-meta)
                           (ce2--codex-user (vector `((type . "input_text") (text . ,(make-string 201 ?界)))))))
    (ce2--check-cache-preview-bound file)))

(ert-deftest ce2-mutation-control-rejects-disabling-preview-bound-at-parser-dispatch ()
  (ce2--file
    (ce2--write file (list (ce2--codex-meta)
                           (ce2--codex-user (vector `((type . "input_text") (text . ,(make-string 201 ?界)))))))
    (ce2--check-cache-preview-bound file)
    (cl-letf (((symbol-function 'agent-sidebar--bounded-preview) #'identity))
      (should-error (ce2--check-cache-preview-bound file) :type 'ert-test-failed))))
