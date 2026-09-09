;;; gap-tests.el --- Shared behavior checks for both packages -*- lexical-binding: t; -*-
;; These checks use synthetic files and stub terminal/session entry points.
;; No user transcript is read or deleted, and no model process is started.
(require 'ert)
(require 'cl-lib)
(require 'json)
(defvar gap-prefix (getenv "GAP_PREFIX"))
(defun gap-symbol (suffix) (intern (concat gap-prefix suffix)))
(defun gap-call (suffix &rest args) (apply (gap-symbol suffix) args))
(defun gap-value (suffix) (symbol-value (gap-symbol suffix)))
(defun gap-json (text &optional model)
  (concat (json-encode `((type . "user") (cwd . "/private/tmp/")
                         (timestamp . "2026-09-08T12:00:00Z")
                         (message . ((content . ,text))))) "\n"
          (when model
            (concat (json-encode `((type . "assistant")
                                  (message . ((model . ,model) (content . "answer"))))) "\n"))))

(defmacro gap-with-entry (contents &rest body)
  (declare (indent 1))
  `(let* ((root (make-temp-file "vegeta-gap-" t))
          (file (expand-file-name "11111111-1111-4111-8111-111111111111.jsonl" root))
          (listings 0) entry)
     (unwind-protect
         (progn
           (with-temp-file file (insert ,contents))
           (cl-progv (mapcar #'gap-symbol '("-refresh-timer" "--parse-cache" "--parse-queue"
                                          "--parse-timer" "-providers" "-enabled-providers"))
               (list nil (make-hash-table :test 'equal) nil nil nil '(claude-cli))
             (with-temp-buffer
               (gap-call "-mode")
               (set (gap-symbol "-providers")
                    (list (cons 'claude-cli
                                (list :id 'claude-cli :name "Claude CLI"
                                      :parse (gap-symbol "--claude-parse")
                                      :list (lambda ()
                                              (cl-incf listings)
                                              (list (list :provider 'claude-cli :id file
                                                          :repo root :agent "Claude"
                                                          :mtime (float-time (file-attribute-modification-time
                                                                              (file-attributes file)))
                                                          :extras (list :session-id (file-name-base file)))))))))
               (cl-letf (((symbol-function (gap-symbol "--start-parse-timer")) #'ignore))
                 (gap-call "-refresh")
                 (setq entry (car (gap-call "--all-entries")))
                 ,@body))))
       (delete-directory root t))))

(ert-deftest gap-common-claude-metadata-and-date-group ()
  (gap-with-entry (gap-json "ordinary first prompt")
    (let ((meta (gap-call "--ensure-parsed" entry)))
      (should (equal "ordinary first prompt" (plist-get meta :preview)))
      (should (equal "/private/tmp/" (plist-get meta :cwd)))
      (should (equal (file-name-base file) (plist-get meta :session-id)))
      (should (equal "2026-09-08" (gap-call "--group-key" entry 'date))))))

(ert-deftest gap-common-mode-and-navigation-bindings ()
  (cl-progv (list (gap-symbol "-refresh-timer")) '(nil)
    (with-temp-buffer
      (gap-call "-mode")
      (should (derived-mode-p 'special-mode))
      (dolist (pair '(("RET" . "-visit") ("o" . "-open-transcript")
                      ("g" . "-refresh") ("TAB" . "-toggle-group")))
        (should (eq (lookup-key (current-local-map) (kbd (car pair)))
                    (gap-symbol (cdr pair))))))))

(ert-deftest gap-codex-provider-and-commands ()
  (should (alist-get 'codex-cli (gap-value "-providers")))
  (dolist (suffix '("-visit-in-agent-shell" "-new-session" "-set-filter" "-set-grouping"))
    (should (commandp (gap-symbol suffix)))))

(ert-deftest gap-parser-model-after-user ()
  (gap-with-entry (gap-json "first prompt" "claude-test-model")
    (should (equal "claude-test-model" (plist-get (gap-call "--ensure-parsed" entry) :model)))))

(ert-deftest gap-preview-storage-bound ()
  (gap-with-entry (gap-json (make-string 5000 ?a))
    (should (<= (length (plist-get (gap-call "--ensure-parsed" entry) :preview)) 200))))

(ert-deftest gap-text-after-tool-result ()
  (gap-with-entry (gap-json [((type . "tool_result") (content . "tool output"))
                            ((type . "text") (text . "actual user text"))])
    (should (equal "actual user text" (plist-get (gap-call "--ensure-parsed" entry) :preview)))))

(ert-deftest gap-cache-rechecks-file-on-action ()
  (gap-with-entry (gap-json "old")
    (gap-call "--ensure-parsed" entry)
    (let ((mtime (file-attribute-modification-time (file-attributes file))))
      (with-temp-file file (insert (gap-json "updated preview with a different size")))
      (set-file-times file mtime))
    (should (equal "updated preview with a different size"
                   (plist-get (gap-call "--ensure-parsed" entry) :preview)))))

(ert-deftest gap-redraw-does-not-rediscover ()
  (gap-with-entry (gap-json "prompt")
    (let ((before listings))
      (dotimes (_ 3) (gap-call "--redraw"))
      (message "GAP redraw discovery calls: %d" (- listings before))
      (should (= before listings)))))

(ert-deftest gap-parse-state-is-buffer-local ()
  (let ((buffer-a (generate-new-buffer " gap-a"))
        (buffer-b (generate-new-buffer " gap-b")))
    (unwind-protect
        (cl-progv (list (gap-symbol "-refresh-timer") (gap-symbol "--parse-queue")) '(nil nil)
          (with-current-buffer buffer-a
            (gap-call "-mode")
            (set (gap-symbol "--parse-queue") '(fixture-a)))
          (with-current-buffer buffer-b
            (gap-call "-mode")
            (should-not (gap-value "--parse-queue"))))
      (kill-buffer buffer-a)
      (kill-buffer buffer-b))))

(ert-deftest gap-ambiguous-claude-recovery-does-not-guess ()
  (let ((time (float-time (date-to-time "2026-09-08T12:00:00Z"))))
    (cl-letf (((symbol-function (gap-symbol "--claude-sessions-for-cwd"))
               (lambda (_) (list (cons "session-a" (+ time 2)) (cons "session-b" (+ time 3))))))
      (should-not (gap-call "--find-claude-session-for-transcript"
                           "Claude" "/private/tmp/" "2026-09-08T12:00:00Z")))))

(ert-deftest gap-open-transcript-uses-view-mode ()
  (gap-with-entry (gap-json "prompt")
    (let ((target nil) (inhibit-read-only t))
      (erase-buffer)
      (insert "row")
      (add-text-properties (point-min) (point-max)
                           (list (gap-symbol "-entry") entry (gap-symbol "-file") file))
      (goto-char (point-min))
      (unwind-protect
          (cl-letf (((symbol-function (gap-symbol "--pop-to")) (lambda (buffer) (setq target buffer))))
            (gap-call "-open-transcript")
            (should (buffer-local-value 'view-mode target)))
        (when (buffer-live-p target) (kill-buffer target))))))

(ert-deftest gap-delete-preserves-open-transcript ()
  (gap-with-entry (gap-json "prompt")
    (let ((owner (generate-new-buffer " gap-owner")) (delete-by-moving-to-trash nil))
      (unwind-protect
          (progn
            (with-current-buffer owner (setq-local agent-shell--transcript-file file))
            (puthash file 'delete (gap-value "--marks"))
            (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list owner)))
                      ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (gap-call "-execute"))
            (should (file-exists-p file))
            (should (eq 'delete (gethash file (gap-value "--marks")))))
        (kill-buffer owner)))))

(ert-deftest gap-repeated-claude-visit-reuses-terminal ()
  (gap-with-entry (gap-json "prompt")
    (let ((launches 0) buffers)
      (unwind-protect
          (cl-letf (((symbol-function (gap-symbol "--launch-terminal"))
                     (lambda (&rest _)
                       (cl-incf launches)
                       (let ((buffer (generate-new-buffer " gap-terminal")))
                         (push buffer buffers) buffer)))
                    ((symbol-function (gap-symbol "--terminal-live-p"))
                     (lambda (buffer) (memq buffer buffers)))
                    ((symbol-function (gap-symbol "--pop-to")) #'ignore))
            (gap-call "--claude-visit" entry)
            (gap-call "--claude-visit" entry)
            (message "GAP terminal launch calls: %d" launches)
            (should (= 1 launches)))
        (mapc #'kill-buffer buffers)))))

;;; gap-tests.el ends here
