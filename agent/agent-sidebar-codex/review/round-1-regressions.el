;;; -*- lexical-binding: t; -*-
(defvar agent-sidebar-review-skip-round-two)
(let ((agent-sidebar-review-skip-round-two t))
  (load (expand-file-name "regressions.el" (file-name-directory load-file-name)) nil t))
