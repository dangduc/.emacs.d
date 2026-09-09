;;; contrarian-evidence-controls.el --- Targeted mutation controls -*- lexical-binding: t; -*-
(load (expand-file-name "contrarian-evidence-probe.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)
(ert-run-tests-batch-and-exit
 '(member contrarian-evidence-byte-limit-is-independent-of-boundary
          contrarian-evidence-first-message-is-a-boundary-without-separator
          contrarian-evidence-complete-id-at-byte-limit-is-retained))
