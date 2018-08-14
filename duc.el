;; Personal Theming override for org-mode
;; Keep org-mode headers from being scaled by themes.
;; Plucked from [Disable enlarged Org mode header appearance]
;;             (https://emacs.stackexchange.com/questions/22584/)
(defun duc/org-mode-theme (&rest _)
  (ignore-errors
    (dolist (face '(org-level-1
                    org-level-2
                    org-level-3
                    org-level-4
                    org-level-5))
      (set-face-attribute face nil
                          :family "iA Writer Duospace"
                          :height 1.0))))

(defmacro duc/alist-replace (list-var element)
  `(let
       ((replaced-list-var
         (assq-delete-all
          (car ',element) ,list-var)))
     (setq ,list-var
           (add-to-list 'replaced-list-var ',element))))

(defmacro duc/alist-replace-set (list-var element)
  `(setq ,list-var (duc/alist-replace ,list-var ,element)))

(defvar duc/font-height)
(setq duc/font-height 140)
(defvar duc/font-height-mode-line)
(setq duc/font-height-mode-line 120)

(defun duc/font-size-increase ()
  (interactive)
  (setq duc/font-height (+ duc/font-height 20))
  (set-face-attribute 'default nil
                      :height duc/font-height)
  (setq duc/font-height-mode-line
        (+ duc/font-height-mode-line 20))
  (dolist (sym '(mode-line mode-line-inactive))
    (set-face-attribute sym nil
     :height duc/font-height-mode-line))
  (print duc/font-height))

(defun duc/font-size-decrease ()
  (interactive)
  (setq duc/font-height (+ duc/font-height -20))
  (set-face-attribute 'default nil
                      :height duc/font-height)
  (setq duc/font-height-mode-line
        (+ duc/font-height-mode-line -20))
  (dolist (sym '(mode-line mode-line-inactive))
    (set-face-attribute sym nil
     :height duc/font-height-mode-line))
  (print duc/font-height))

(defun duc/font-size ()
  (interactive)
  (setq duc/font-height (string-to-number (completing-read "font size: "
                                         '("140"))))
  (set-face-attribute 'default nil
                      :height duc/font-height
                      :weight 'normal
                      :width 'normal))

;; font chooser
(defun duc/ivy-font ()
  (interactive)
  (set-face-attribute 'default nil
                      :family (completing-read "font: "
                                               (font-family-list))
                      :height duc/font-height
                      :weight 'normal
                      :width 'normal))

(defun duc/ivy-shell ()
  (interactive)
  (let ((terminal-buffers (seq-filter (lambda (x)
                                        (string-match-p
                                         (regexp-quote "terminal-") x))
                                      (mapcar (function buffer-name) (buffer-list)))))
    (let ((buffer-name (completing-read "shell : " terminal-buffers)))
     (if (member buffer-name terminal-buffers)
         (switch-to-buffer buffer-name)
       (ansi-term "/bin/zsh" (concat "terminal-" buffer-name))))))

(defun duc/sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

(defun duc/eval-dwim (p)
  (interactive "P")
  (pcase major-mode
    ('racket-mode (racket-run))
    ('emacs-lisp-mode (eval-last-sexp p))
    (_ (eval-last-sexp p))))

(defun duc/eval-buffer ()
  (interactive)
  (pcase major-mode
    ('racket-mode (racket-run))
    ('emacs-lisp-mode (eval-buffer))
    (_ (eval-buffer))))

(defun duc/eval-last (p)
  (interactive "P")
  (pcase major-mode
    ('emacs-lisp-mode (eval-last-sexp p))
    (_ (eval-last-sexp p))))

(defvar-local duc/header-line-format nil)
(defun duc/mode-line-in-header ()
  "Toggle displaying modeline in header instead of footer
https://emacs-doctor.com/emacs-strip-tease.html"
  (interactive)
  (if (not header-line-format)
      (setq header-line-format mode-line-format
            mode-line-format nil)
    (setq mode-line-format header-line-format
          header-line-format nil))
  (set-window-buffer nil (current-buffer)))
