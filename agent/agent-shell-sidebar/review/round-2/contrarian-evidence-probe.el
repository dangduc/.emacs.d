;;; contrarian-evidence-probe.el --- Independent boundary checks -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)

(defmacro ce2--file (&rest body)
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "sidebar-ce2-" t)))
          (file (expand-file-name ".agent-shell/transcripts/fixture.md" root))
          (default-directory root)
          (coding-system-for-write 'utf-8-unix))
     (unwind-protect
         (progn (make-directory (file-name-directory file) t) ,@body)
       (delete-directory root t))))

(defun ce2--padding (bytes multibyte)
  (if multibyte
      (concat (make-string (/ bytes 2) ?é) (make-string (% bytes 2) ?x))
    (make-string bytes ?x)))

(ert-deftest ce2-cutoff-matrix-distinguishes-bytes-lines-and-eof ()
  (ce2--file
    (dolist (multibyte '(nil t))
      (dolist (newline '(nil t))
        (dolist (file-continues '(nil t))
          (dolist (length '(8191 8192 8193))
            (let* ((tail (concat "\n**Session ID:** intact" (when newline "\n")))
                   (body (concat (ce2--padding (- length (string-bytes tail)) multibyte) tail)))
              (should (= (string-bytes body) length))
              (with-temp-file file
                (insert body (if file-continues "-continuation\n---\n" "")))
              (let* ((header (agent-shell-sidebar--parse-header-from-file file))
                     (truncated (or file-continues (> length 8192)))
                     (complete (and (<= length 8192) (or newline (not truncated)))))
                (message "CE2-MATRIX utf8=%S newline=%S continues=%S bytes=%d header=%S"
                         multibyte newline file-continues length header)
                (should (equal (plist-get header :session-id) (when complete "intact")))
                (should (eq (not (null (plist-get header :error))) truncated))))))))))

(ert-deftest ce2-first-metadata-boundary-wins ()
  (ce2--file
    (dolist (boundary '("---\n" "## User (now)\n" "## Agent (now)\n"))
      (with-temp-file file
        (insert "**Agent:** Before\n" boundary
                "**Session ID:** prompt-content\n**Working Directory:** /wrong/\n---\n"))
      (let ((header (agent-shell-sidebar--parse-header-from-file file)))
        (should (equal (plist-get header :agent) "Before"))
        (should-not (plist-get header :session-id))
        (should-not (plist-get header :cwd))))))

(ert-deftest ce2-bounded-read-excludes-late-field-without-any-boundary ()
  (ce2--file
    (with-temp-file file
      (insert "**Agent:** Before\n" (make-string 8300 ?x)
              "\n**Session ID:** past-limit\n"))
    (let ((header (agent-shell-sidebar--parse-header-from-file file)))
      (should (equal (plist-get header :agent) "Before"))
      (should-not (plist-get header :session-id))
      (should (plist-get header :error)))))

(ert-deftest ce2-real-writer-keeps-tab-and-space-path-suffix ()
  (ce2--file
    (let* ((directory (expand-file-name "project\t " root))
           (trimmed (expand-file-name "project" root))
           (agent-shell-cwd-function (lambda () directory)))
      (make-directory directory)
      (make-directory trimmed)
      (with-temp-buffer
        (setq major-mode 'agent-shell-mode)
        (setq-local agent-shell--transcript-file file)
        (setq-local agent-shell--state
                    '((:agent-config . ((:mode-line-name . "Fixture")))
                      (:session . ((:id . "existing")))))
        (should (agent-shell--ensure-transcript-file)))
      (let ((header (agent-shell-sidebar--parse-header-from-file file)))
        (should (equal (plist-get header :cwd) directory))
        (should (equal (agent-shell-sidebar--working-directory file header)
                       (file-name-as-directory directory)))))))

(message "CE2 SOURCE BEFORE=%S" (symbol-file 'agent-shell-sidebar--parse-header-from-file 'defun))
(add-hook 'kill-emacs-hook
          (lambda () (message "CE2 SOURCE AFTER=%S"
                              (symbol-file 'agent-shell-sidebar--parse-header-from-file 'defun))))
