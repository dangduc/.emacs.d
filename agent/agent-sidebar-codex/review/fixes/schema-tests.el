;;; schema-tests.el --- Content shape recovery checks -*- lexical-binding: t; -*-
(load (expand-file-name "../round-2/contrarian-evidence-tests.el" (file-name-directory load-file-name)) nil t)

(ert-deftest schema-codex-valid-block-objects-in-arrays-retain-identity-and-cwd ()
  (ce2--file
    (ce2--write file
                (list (ce2--codex-meta)
                      (ce2--codex-user
                       [7 "ignored scalar" nil
                        ((type . "input_image") (image_url . "fixture-only"))
                        ((type . "input_text") (text . ((unsupported . 42))))
                        ((type . "input_text") (text . "SUPPORTED CODEX ARRAY"))])))
    (let ((meta (agent-sidebar--codex-parse (list :id file))))
      (should (equal (plist-get meta :session-id) ce2--uuid))
      (should (equal (plist-get meta :cwd) temporary-file-directory))
      (should (equal (plist-get meta :preview) "SUPPORTED CODEX ARRAY")))))

(ert-deftest schema-claude-valid-block-objects-in-arrays-retain-identity-and-cwd ()
  (ce2--file
    (ce2--write file
                (list (ce2--claude-user
                       [7 "ignored scalar" nil
                        ((type . "tool_result") (content . "Fixture tool result"))
                        ((type . "text") (text . ((unsupported . 42))))
                        ((type . "text") (text . "SUPPORTED CLAUDE ARRAY"))])))
    (let ((meta (agent-sidebar--claude-parse (list :id file :extras (list :session-id ce2--uuid)))))
      (should (equal (plist-get meta :session-id) ce2--uuid))
      (should (equal (plist-get meta :cwd) temporary-file-directory))
      (should (equal (plist-get meta :preview) "SUPPORTED CLAUDE ARRAY")))))

(ert-deftest schema-claude-string-content-remains-supported ()
  (ce2--file
    (ce2--write file (list (ce2--claude-user "SUPPORTED CLAUDE STRING")))
    (let ((meta (agent-sidebar--claude-parse (list :id file :extras (list :session-id ce2--uuid)))))
      (should (equal (plist-get meta :cwd) temporary-file-directory))
      (should (equal (plist-get meta :preview) "SUPPORTED CLAUDE STRING")))))

(ert-deftest schema-codex-unsupported-content-object-does-not-discard-earlier-cwd ()
  (ce2--file
    (ce2--write file (list (ce2--codex-meta)
                           (ce2--codex-user '((type . "input_text") (text . "Unsupported shape")))) )
    (let ((meta (agent-sidebar--codex-parse (list :id file))))
      (should-not (plist-get meta :error))
      (should (equal (plist-get meta :session-id) ce2--uuid))
      (should (equal (plist-get meta :cwd) temporary-file-directory))
      (should-not (plist-get meta :preview)))))
