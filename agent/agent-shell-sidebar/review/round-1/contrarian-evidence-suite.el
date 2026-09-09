;;; contrarian-evidence-suite.el --- Run the existing suite -*- lexical-binding: t; -*-
(load (expand-file-name "../../tests/sidebar-tests.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)
