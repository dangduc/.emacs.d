;;; contrarian-evidence-tests.el --- Independent preview oracles -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'json)

(defconst ce--own-id "11111111-1111-7111-8111-111111111111")
(defconst ce--parent-id "22222222-2222-7222-8222-222222222222")
(defun ce--meta (id cwd)
  `((type . "session_meta") (payload . ((id . ,id) (cwd . ,cwd) (timestamp . "2026-09-08T12:00:00Z")))))
(defun ce--user (&rest strings)
  `((type . "response_item") (payload . ((type . "message") (role . "user")
    (content . ,(vconcat (mapcar (lambda (text) `((type . "input_text") (text . ,text))) strings)))))))
(defun ce--event (text)
  `((type . "event_msg") (payload . ((type . "user_message") (message . ,text)))))
(defun ce--parse (&rest records)
  (let ((file (make-temp-file "ce-native-" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert (json-encode (ce--meta ce--own-id temporary-file-directory)) "\n")
            (dolist (record records) (insert (json-encode record) "\n")))
          (agent-sidebar--codex-parse (list :id file)))
      (delete-file file))))
(defun ce--expect-preview (expected &rest records)
  (let ((meta (apply #'ce--parse records)))
    (should-not (plist-get meta :error))
    (should (equal (plist-get meta :session-id) ce--own-id))
    (should (equal (plist-get meta :preview) expected))))

(ert-deftest ce-first-prompt-survives-later-event-from-a-new-turn ()
  ;; First turn is saved by a response-item writer; after resuming, a later
  ;; writer supplies response_item plus event_msg.  File order is authoritative.
  (ce--expect-preview "FIRST REQUEST"
                      (ce--user "FIRST REQUEST")
                      '((type . "turn_context") (payload . ((model . "fixture-model"))))
                      (ce--user "SECOND REQUEST")
                      (ce--event "SECOND REQUEST")))

(ert-deftest ce-plugin-inventory-is-context-not-the-first-prompt ()
  ;; Mirrors an observed native shape: the entire first input_text block is a
  ;; recommended_plugins wrapper; a subsequent response_item holds the request.
  (ce--expect-preview "FIRST REQUEST"
                      (ce--user "<recommended_plugins>\nFixture plugin list\n</recommended_plugins>")
                      (ce--user "# AGENTS.md instructions\nFixture repository guidance"
                                "<environment_context>fixture</environment_context>")
                      (ce--user "FIRST REQUEST")))

(ert-deftest ce-inline-plugin-markup-is-user-text ()
  ;; A context filter must not discard arbitrary quoted/non-prefix user text.
  (ce--expect-preview "Explain <recommended_plugins> in this example"
                      (ce--user "Explain <recommended_plugins> in this example")))

(ert-deftest ce-event-only-history-is-supported ()
  (ce--expect-preview "EVENT REQUEST" (ce--event "EVENT REQUEST")))

(ert-deftest ce-fork-identity-is-not-inherited-parent-identity ()
  (let ((meta (ce--parse (ce--meta ce--parent-id "/fixture-parent/")
                        (ce--user "PARENT REQUEST") (ce--user "FORK REQUEST"))))
    (should (equal (plist-get meta :session-id) ce--own-id))
    (should (equal (plist-get meta :cwd) temporary-file-directory))
    (should (equal (plist-get meta :preview) "PARENT REQUEST"))))

(ert-deftest ce-mutation-control-kills-disabled-context-filter ()
  ;; A negative control for the independent fixture and assertion: bypassing
  ;; context rejection must be detected, even when all regular cases pass.
  (ce--expect-preview "FIRST REQUEST"
                      (ce--user "<environment_context>fixture</environment_context>" "FIRST REQUEST"))
  (cl-letf (((symbol-function 'agent-sidebar--codex-user-text)
             (lambda (content) (alist-get 'text (car content)))))
    (should-error
     (ce--expect-preview "FIRST REQUEST"
                        (ce--user "<environment_context>fixture</environment_context>" "FIRST REQUEST"))
     :type 'ert-test-failed)))
