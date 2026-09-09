;;; contrarian-workflow-native-policy.el --- Isolated harness controls -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(defvar native-comp-enable-subr-trampolines)
(setq native-comp-enable-subr-trampolines t)
(let ((native-comp-enable-subr-trampolines native-comp-enable-subr-trampolines))
  (load (expand-file-name "contrarian-workflow.el"
                          (file-name-directory load-file-name)) nil nil t))
(unless native-comp-enable-subr-trampolines
  (error "Workflow test loading leaked its native compilation policy"))
(message "CW2 native policy before ERT: %S" native-comp-enable-subr-trampolines)
(add-hook 'kill-emacs-hook
          (lambda () (message "CW2 native policy after ERT: %S"
                              native-comp-enable-subr-trampolines)))
(if (equal (getenv "CW2_NATIVE_CONTROL") "fixture-delete")
    (cl-letf (((symbol-function 'agent-shell-sidebar-execute)
               (lambda ()
                 (let ((target (car (hash-table-keys agent-shell-sidebar--marks))))
                   (message "CW2 negative control attempts fixture deletion: %s" target)
                   (delete-file target)))))
      (ert-run-tests-batch-and-exit
       "^cw2-cancel-deletion-while-metadata-pending-keeps-marks-and-files$"))
  (ert-run-tests-batch-and-exit
   (if (equal (getenv "CW2_NATIVE_CONTROL") "partial-delete")
       "^cw2-delete-partial-success-retains-failed-visible-mark$"
     "^cw2-")))
