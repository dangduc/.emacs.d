;;; regressions.el --- Combined provider review regression gate -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(defvar agent-sidebar-review-skip-round-two nil)
(let* ((directory (file-name-directory load-file-name))
       (test-root (expand-file-name "../../" directory))
       (seen (make-hash-table :test 'equal))
       (original-load (symbol-function 'load)))
  ;; Probes remain independently runnable and load common fixture files.
  ;; Emacs 31 rejects duplicate ERT definitions, so load those files once
  ;; during registration. Restore ordinary load before any test executes.
  (cl-letf (((symbol-function 'load)
             (lambda (file &rest arguments)
               (if (and (stringp file) (file-name-absolute-p file)
                        (string-prefix-p test-root (expand-file-name file)))
                   (let ((key (file-truename file)))
                     (if (gethash key seen) t
                       (puthash key t seen)
                       (apply original-load file arguments)))
                 (apply original-load file arguments)))))
  (dolist (file '("fixes/acp-tests.el" "fixes/terminal-tests.el"
                  "fixes/parser-tests.el" "fixes/workflow-tests.el"
                  "fixes/ownership-tests.el" "fixes/performance-tests.el"
                  "fixes/display-tests.el" "fixes/group-membership-tests.el"
                  "fixes/local-cwd-tests.el" "fixes/ownership-transition-tests.el"
                  "fixes/schema-tests.el"
                  "root-integration-tests.el" "round-1/kingsbury-tests.el"))
    (load (expand-file-name file directory) nil t))
  ;; Historical performance probes intentionally assert the old defect size.
  ;; Their observations are preserved in Round 1; final tests assert bounds.
  (unless agent-sidebar-review-skip-round-two
    (dolist (file (directory-files (expand-file-name "round-2/" directory) t "-tests\\.el\\'"))
      (load file nil t)))
  (dolist (name '(luu-cold-warm-cost-and-record-work
                   luu-large-preview-materializes-megabytes
                   luu-large-preview-synchronous-filter-scaling
                   luu-typical-injected-context-cost
                   luu-preview-bound-positive-control
                   luu-r2-cold-mixed-group-parses-after-ambiguity-is-proven))
    (when (ert-test-boundp name) (ert-delete-test name))))
)
