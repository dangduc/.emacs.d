;;; -*- lexical-binding: t; -*-
(require 'ert)
(defvar ce-policy-before native-comp-enable-subr-trampolines)
(load "/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/fixes/identity-tests.el" nil t)
(load "/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/contrarian-workflow.el" nil t)
(message "POLICY before=%S after=%S selector-native=%S source=%S" ce-policy-before native-comp-enable-subr-trampolines (subr-native-elisp-p (symbol-function 'agent-shell-select-config)) (symbol-file 'agent-shell-sidebar--parse-header-from-file 'defun))
(ert-run-tests-batch-and-exit 'identity-ambiguous-alias-obeys-explicit-selection-on-every-visit)
