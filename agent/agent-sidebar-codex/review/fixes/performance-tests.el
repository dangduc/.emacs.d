;;; performance-tests.el --- Bounded preview regressions -*- lexical-binding: t; -*-
(load "/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-1/luu-tests.el" nil t)

(ert-deftest performance-preview-boundary-unicode-and-provider-contract ()
  "Normalize provider results without mutating their own metadata object."
  (merge--fixture
    (let* ((file (merge--markdown root "contract" "original-session"))
           (entry (list :id file :provider 'preview-fixture))
           (agent-sidebar-providers (copy-tree agent-sidebar-providers)))
      (puthash file entry agent-sidebar--entries)
      (dolist (preview (list nil "" "short prompt" (make-string 199 ?x)
                            (make-string 200 ?😀) (make-string 201 ?😀)
                            (concat (make-string 199 ?é) "ends after boundary")))
        (let ((provider-meta (list :preview preview :session-id "original-session" :model "model")))
          (agent-sidebar-register-provider
           (list :id 'preview-fixture :name "Fixture"
                 :list (lambda () (list entry)) :visit #'ignore
                 :parse (lambda (_entry) provider-meta)))
          (let* ((meta (agent-sidebar--parse-file file))
                 (actual (plist-get meta :preview)))
            (should (equal (plist-get meta :session-id) "original-session"))
            (should (equal (plist-get meta :model) "model"))
            (should (eq (plist-get meta :provider) 'preview-fixture))
            (should (eq (plist-get provider-meta :preview) preview))
            (if (and preview (> (length preview) 200))
                (progn
                  (should (= (length actual) 200))
                  (should (equal actual (concat (substring preview 0 199) "…")))
                  (should (equal actual (decode-coding-string
                                         (encode-coding-string actual 'utf-8) 'utf-8))))
              (should (eq actual preview)))))))))

(ert-deftest performance-preview-bound-applies-to-three-built-in-providers ()
  "The shared dispatcher bounds Markdown, Claude JSONL, and Codex JSONL."
  (codex-test--fixture
    (let* ((text (concat (make-string 500 ?p) "TAIL-SENTINEL"))
           (markdown (merge--markdown root "long" "markdown-session"))
           (claude (merge--json-file root store "claude-session"
                      `(((type . "user") (cwd . ,root)
                         (message . ((content . ,text)))))))
           (codex (codex-test--write agent-sidebar-codex-home root codex-test--id
                     `(((type . "session_meta") (payload . ((id . ,codex-test--id) (cwd . ,root))))
                       ((type . "event_msg") (payload . ((type . "user_message") (message . ,text))))))))
      (with-temp-file markdown
        (insert (format "**Agent:** Claude\n**Working Directory:** %s\n**Session ID:** markdown-session\n---\n## User\n\n%s\n" root text)))
      (agent-sidebar-refresh)
      (merge--drain)
      (should (= (length (merge--rows)) 3))
      (dolist (file (list markdown claude codex))
        (let ((preview (plist-get (agent-sidebar--cached-meta (gethash file agent-sidebar--entries)) :preview)))
          (should (= (length preview) 200))
          (should (equal preview (concat (make-string 199 ?p) "…"))))
        (with-temp-buffer
          (insert-file-contents file)
          (should (string-search "TAIL-SENTINEL" (buffer-string)))))
      (agent-sidebar-set-filter "TAIL-SENTINEL")
      (should-not (merge--rows))
      (agent-sidebar-set-filter (make-string 40 ?p))
      (should (= (length (merge--rows)) 3)))))

(ert-deftest performance-300-long-previews-bounded-cache-and-full-transcript ()
  "Keep the 300-session stress fixture below 100 KB without rewriting files."
  (codex-test--fixture
    (let ((agent-sidebar-enabled-providers '(codex-cli))
          (calls 0)
          (parser (symbol-function 'agent-sidebar--codex-parse)))
      (luu--rollouts 300 'large-preview agent-sidebar-codex-home root)
      (garbage-collect)
      (cl-letf (((symbol-function 'agent-sidebar--codex-parse)
                 (lambda (entry) (cl-incf calls) (funcall parser entry))))
        (agent-sidebar-refresh)
        (merge--drain)
        (should (= calls 300))
        (should (= (length (merge--rows)) 300))
        (should (< (buffer-size) 100000))
        (dolist (entry agent-sidebar--entry-list)
          (let* ((meta (agent-sidebar--cached-meta entry))
                 (file (plist-get entry :id)))
            (should (= (length (plist-get meta :preview)) 200))
            (should (equal (plist-get meta :model) "fixture-model"))
            (should (equal (plist-get meta :cwd) root))
            (should (string-search (plist-get meta :session-id) file))
            (should (> (file-attribute-size (file-attributes file)) 200000))))
        (let ((size (buffer-size))
              (warm (car (benchmark-run 1 (agent-sidebar-refresh))))
              (filter-time (car (benchmark-run 1 (agent-sidebar-set-filter "fixture-model")))))
          (should (= calls 300))
          (should-not agent-sidebar--parse-queue)
          (should (= (length (merge--rows)) 300))
          (message "PERFORMANCE-FIX %S"
                   (list :sessions 300 :buffer-characters size :warm-refresh warm
                         :filter-seconds filter-time :parser-calls calls)))
        (let* ((file (plist-get (car agent-sidebar--entry-list) :id))
               (checksum (with-temp-buffer
                           (insert-file-contents-literally file)
                           (secure-hash 'sha256 (current-buffer))))
               opened)
          (merge--goto file)
          (cl-letf (((symbol-function 'agent-shell-sidebar--pop-to)
                     (lambda (target) (setq opened target))))
            (agent-sidebar-open-transcript))
          (unwind-protect
              (with-current-buffer opened
                (should view-mode)
                (should (> (buffer-size) 200000))
                (should (string-search (make-string 200000 ?p) (buffer-string)))
                (should (equal checksum (secure-hash 'sha256 (current-buffer)))))
            (when (buffer-live-p opened) (kill-buffer opened))))))))
