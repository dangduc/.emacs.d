;;; luu-tests.el --- Synthetic sidebar cost and cache probes -*- lexical-binding: t; -*-
(load "/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/tests.el" nil t)
(require 'benchmark)

(defun luu--rollouts (count kind home cwd)
  "Write COUNT synthetic Codex rollouts of KIND under HOME."
  (let* ((directory (expand-file-name "sessions/2026/09/08" home))
         (padding (json-encode '((type . "event_msg") (payload . ((type . "token_count") (info . nil))))))
         (preview (if (eq kind 'large-preview) (make-string 200000 ?p) "Synthetic prompt")))
    (make-directory directory t)
    (dotimes (n count)
      (let ((id (format "12345678-1234-7123-8123-%012d" n)))
        (with-temp-file (expand-file-name (format "rollout-%s.jsonl" id) directory)
          (insert (json-encode `((type . "session_meta") (payload . ((id . ,id) (cwd . ,cwd)
                                       (timestamp . "2026-09-08T00:00:00Z")
                                       (base_instructions . ((text . ,(if (eq kind 'typical-context) (make-string 20000 ?i) "fixture")))))))) "\n"
                  (json-encode '((type . "turn_context") (payload . ((model . "fixture-model"))))) "\n"
                  (json-encode `((type . "event_msg") (payload . ((type . "user_message") (message . ,preview))))) "\n")
          (when (eq kind 'typical-context)
            (insert (json-encode `((type . "response_item") (payload . ((type . "message") (role . "user")
                      (content . [((type . "input_text") (text . ,(concat "# AGENTS.md instructions\n" (make-string 100000 ?c))))]))))) "\n"))
          (when (eq kind 'dense-events)
            (while (< (buffer-size) 270000) (insert padding "\n"))))))))

(defun luu--measure (count kind)
  (codex-test--fixture
    (let ((agent-sidebar-enabled-providers '(codex-cli))
          (agent-sidebar-parse-chunk-size 30)
          (parses 0) ticks (json-count 0) (max-file-seconds 0.0)
          (orig-parser (symbol-function 'agent-sidebar--codex-parse))
          (orig-json (symbol-function 'json-parse-string)))
      (luu--rollouts count kind agent-sidebar-codex-home root)
      (garbage-collect)
      (cl-letf (((symbol-function 'agent-sidebar--codex-parse)
                 (lambda (entry) (cl-incf parses)
                   (let ((started (float-time)))
                     (prog1 (funcall orig-parser entry)
                       (setq max-file-seconds (max max-file-seconds (- (float-time) started)))))))
                ((symbol-function 'json-parse-string)
                 (lambda (&rest args) (cl-incf json-count) (apply orig-json args))))
        (let ((cold-refresh (car (benchmark-run 1 (agent-sidebar-refresh)))))
          (while agent-sidebar--parse-timer
            (let ((before parses))
              (push (list :seconds (car (benchmark-run 1 (timer-event-handler agent-sidebar--parse-timer)))
                          :parses (- parses before)) ticks)))
          (should (= count (length (merge--rows))))
          (let* ((size (buffer-size))
                 (parses-before parses)
                 (warm (car (benchmark-run 1 (agent-sidebar-refresh))))
                 (filtered (car (benchmark-run 1 (agent-sidebar-set-filter "fixture-model")))))
            (should (= parses-before parses))
            (should-not agent-sidebar--parse-queue)
            (message "LUU-MEASURE %S" (list :kind kind :count count :cold-refresh cold-refresh
                    :ticks (nreverse ticks) :buffer-chars size :warm-refresh warm :filter filtered
                    :parses parses :json-records json-count :max-file-seconds max-file-seconds))
            (list :size size :ticks ticks :json-count json-count :filter-seconds filtered)))))))

(ert-deftest luu-cold-warm-cost-and-record-work ()
  "Measure real work for complete early metadata plus irrelevant later records."
  (let ((short (luu--measure 30 'small))
        (dense (luu--measure 30 'dense-events)))
    (should (= (plist-get short :json-count) 90))
    (should (> (plist-get dense :json-count) 50000))))

(ert-deftest luu-large-preview-materializes-megabytes ()
  "A first-line pasted payload should not balloon sidebar display storage."
  (let ((short (luu--measure 30 'small))
        (large (luu--measure 30 'large-preview)))
    (should (< (plist-get short :size) 10000))
    ;; Reproduction assertion for the current problem, not a claimed fix oracle.
    (should (> (plist-get large :size) 6000000))))

(ert-deftest luu-large-preview-synchronous-filter-scaling ()
  "Measure three hundred valid long first lines against small prompts."
  (let ((short (luu--measure 300 'small))
        (large (luu--measure 300 'large-preview)))
    (should (< (plist-get short :size) 20000))
    (should (> (plist-get large :size) 60000000))))

(ert-deftest luu-typical-injected-context-cost ()
  "Measure 300 synthetic rollouts with 20 KiB instructions and 100 KiB context."
  (let ((result (luu--measure 300 'typical-context)))
    (should (< (plist-get result :size) 20000))))

(ert-deftest luu-preview-bound-positive-control ()
  "A 200-character display preview eliminates oversized rows, preserving IDs."
  (let ((parser (symbol-function 'agent-sidebar--codex-parse)))
    (cl-letf (((symbol-function 'agent-sidebar--codex-parse)
               (lambda (entry)
                 (let* ((meta (funcall parser entry)) (preview (plist-get meta :preview)))
                   (when preview
                     (setq meta (plist-put meta :preview (substring preview 0 (min 200 (length preview))))))
                   meta))))
      (let ((result (luu--measure 300 'large-preview)))
        (should (< (plist-get result :size) 70000))))))
