;;; batch-package-tests.el --- Exercise an installed archive -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(let* ((installed (pop command-line-args-left))
       (kind (pop command-line-args-left))
       (load-path (cons installed load-path)))
  (require 'fzf-native)
  (if (equal kind "bundled")
      (fzf-native-load-dyn)
    (module-load (expand-file-name "source-built/fzf-native-module.so" installed))
    (fzf-native--verify-initialized-module)
    (setq fzf-native-loaded t))
  (load "/private/tmp/fzf-native-melpa-license-notices/fzf-native-test.el" nil t)
  (load "/private/tmp/fzf-native-melpa-license-notices/fzf-native-utf8-test.el" nil t)
  (princ (format "Installed=%s\nModule=%s\nEmacs=%s PID=%s\n"
                 installed kind emacs-version (emacs-pid)))
  (ert-run-tests-batch-and-exit
   '(member fzf-native-score-with-default-slab-test
            fzf-native-score-case-mode-smart-test
            fzf-native-utf8-exact-match-test
            fzf-native-utf8-fuzzy-match-test
            fzf-native-utf8-case-ignore-length-changing-fold-test
            fzf-native-utf8-normalization-test
            fzf-native-utf8-position-test
            fzf-native-utf8-score-all-test)))
