;;; parser-tests.el --- First prompt fixes and boundary checks -*- lexical-binding: t; -*-
(load (expand-file-name "../round-1/contrarian-evidence-tests.el" (file-name-directory load-file-name)) nil t)

(ert-deftest ce-plugin-wrapper-keeps-a-request-in-its-suffix ()
  (ce--expect-preview "FIRST REQUEST"
                      (ce--user "<recommended_plugins>fixture</recommended_plugins>\n\n FIRST REQUEST\nsecond line")))

(ert-deftest ce-leading-context-wrappers-keep-a-request-after-the-last-close ()
  (ce--expect-preview "FIRST REQUEST"
                      (ce--user "<recommended_plugins>fixture</recommended_plugins>\n<environment_context>fixture</environment_context>\nFIRST REQUEST")))

(ert-deftest ce-standalone-and-incomplete-context-blocks-do-not-win-preview ()
  (ce--expect-preview "FIRST REQUEST"
                      (ce--user "<recommended_plugins>fixture</recommended_plugins>"
                                "<environment_context>unfinished injected context"
                                "FIRST REQUEST")))

(ert-deftest ce-event-context-is-filtered-before-first-real-prompt ()
  (ce--expect-preview "FIRST REQUEST"
                      (ce--event "<recommended_plugins>fixture</recommended_plugins>")
                      (ce--event "<environment_context>fixture</environment_context>")
                      (ce--user "FIRST REQUEST")
                      (ce--event "SECOND REQUEST")))

(ert-deftest ce-event-first-history-and-duplicate-forms-keep-first-request ()
  (ce--expect-preview "FIRST REQUEST"
                      (ce--event "FIRST REQUEST")
                      (ce--user "FIRST REQUEST")
                      (ce--user "SECOND REQUEST")
                      (ce--event "SECOND REQUEST")))

(ert-deftest ce-unknown-tags-and-quoted-leading-tags-remain-user-text ()
  (ce--expect-preview "<user_example>FIRST REQUEST</user_example>"
                      (ce--user "<user_example>FIRST REQUEST</user_example>"))
  (ce--expect-preview "\"<recommended_plugins>\" means what?"
                      (ce--user "\"<recommended_plugins>\" means what?")))

(ert-deftest ce-empty-native-records-do-not-consume-the-first-prompt-slot ()
  (ce--expect-preview "FIRST REQUEST"
                      (ce--user "   ") (ce--event "\n  ")
                      (ce--user "FIRST REQUEST") (ce--event "SECOND REQUEST")))

(ert-deftest ce-many-leading-context-wrappers-retain-the-final-request ()
  ;; Exercise multiple offset advances without requiring repeated suffix copies.
  (ce--expect-preview "FIRST REQUEST"
                      (ce--user (concat (apply #'concat (make-list 200 "<recommended_plugins>x</recommended_plugins>\n"))
                                        "FIRST REQUEST"))))
