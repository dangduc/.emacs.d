;;; luu-tests.el --- Round 2 group action work probes -*- lexical-binding: t; -*-
(load "/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/tests.el" nil t)
(require 'benchmark)

(defun luu-r2--write-groups (home directories count dense)
  "Write COUNT stable files alternating DIRECTORIES, adding DENSE events."
  (let ((directory (expand-file-name "sessions/2026/09/08" home))
        (padding (json-encode '((type . "event_msg") (payload . ((type . "token_count") (info . nil)))))))
    (make-directory directory t)
    (dotimes (n count)
      (let* ((id (format "12345678-1234-7123-8123-%012d" n))
             (file (expand-file-name (format "rollout-%s.jsonl" id) directory))
             (cwd (nth (% n (length directories)) directories)))
        (with-temp-file file
          (dolist (record
                   `(((type . "session_meta")
                      (payload . ((id . ,id) (cwd . ,cwd)
                                  (timestamp . "2026-09-08T00:00:00Z")
                                  (base_instructions . ((text . ,(make-string 20000 ?i)))))))
                     ((type . "turn_context") (payload . ((model . "fixture-model"))))
                     ((type . "event_msg") (payload . ((type . "user_message") (message . "Synthetic prompt"))))))
            (insert (json-encode record) "\n"))
          (when dense
            (while (< (buffer-size) 270000) (insert padding "\n"))))
        ;; Stable, unique mtimes ensure the first two displayed files differ.
        (set-file-times file (seconds-to-time (+ 1800000000 n)))))))

(defun luu-r2--ambiguous-group-measure (count dense early-control)
  "Measure a cold group action, optionally enabling an EARLY-CONTROL guard."
  (codex-test--fixture
    (let* ((agent-sidebar-enabled-providers '(codex-cli))
           (other (expand-file-name "second-directory/" temporary))
           (calls 0) contexts first-ambiguous-at
           (parser (symbol-function 'agent-sidebar--codex-parse))
           (context-function (symbol-function 'agent-sidebar--new-context-for-entry)))
      (make-directory other t)
      (luu-r2--write-groups agent-sidebar-codex-home (list root other) count dense)
      (agent-sidebar-refresh)
      (should (= (length agent-sidebar--parse-queue) count))
      (garbage-collect)
      (cl-letf (((symbol-function 'agent-sidebar--codex-parse)
                 (lambda (entry) (cl-incf calls) (funcall parser entry)))
                ((symbol-function 'agent-sidebar--new-context-for-entry)
                 (lambda (entry)
                   (let ((context (funcall context-function entry)))
                     (cl-pushnew context contexts :test #'equal)
                     (when (> (length contexts) 1)
                       (unless first-ambiguous-at (setq first-ambiguous-at calls))
                       (when early-control
                         (user-error "Group has multiple session contexts; select a transcript row")))
                     context))))
        (let ((elapsed (car (benchmark-run 1
                              (should-error (agent-sidebar--new-context-for-group '((package . codex-cli)))
                                            :type 'user-error)))))
          (should (= first-ambiguous-at 2))
          (should (= (length contexts) 2))
          (message "LUU-R2-GROUP %S" (list :files count :dense dense :early-control early-control
                       :elapsed elapsed :parser-calls calls :ambiguity-established-at first-ambiguous-at))
          (list :elapsed elapsed :calls calls))))))

(ert-deftest luu-r2-cold-mixed-group-parses-after-ambiguity-is-proven ()
  "A mixed package header keeps parsing after the second distinct context."
  (let ((baseline (luu-r2--ambiguous-group-measure 300 t nil))
        (control (luu-r2--ambiguous-group-measure 300 t t)))
    ;; Diagnostic reproduction of the frozen Round 2 source.
    (should (= (plist-get baseline :calls) 300))
    (should (= (plist-get control :calls) 2))))

(ert-deftest luu-r2-warm-unambiguous-group-reuses-metadata-at-300-sessions ()
  "A warm group selects the one context without reading or rediscovering files."
  (codex-test--fixture
    (let ((agent-sidebar-enabled-providers '(codex-cli))
          (calls 0) (discoveries 0)
          (parser (symbol-function 'agent-sidebar--codex-parse))
          (lister (symbol-function 'agent-sidebar--codex-list)))
      (luu-r2--write-groups agent-sidebar-codex-home (list root) 300 nil)
      (agent-sidebar-refresh)
      (merge--drain)
      (cl-letf (((symbol-function 'agent-sidebar--codex-parse)
                 (lambda (entry) (cl-incf calls) (funcall parser entry)))
                ((symbol-function 'agent-sidebar--codex-list)
                 (lambda () (cl-incf discoveries) (funcall lister))))
        (let* (context
               (elapsed (car (benchmark-run 1
                               (setq context (agent-sidebar--new-context-for-group '((package . codex-cli))))))))
          (should (eq (plist-get context :provider) 'codex-cli))
          (should (equal (plist-get context :directory) root))
          (should (equal (plist-get context :codex-home) agent-sidebar-codex-home))
          (should (= calls 0))
          (should (= discoveries 0))
          (message "LUU-R2-WARM %S" (list :files 300 :elapsed elapsed :parses calls :discoveries discoveries)))))))
