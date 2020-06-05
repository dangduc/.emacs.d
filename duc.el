(defmacro duc/alist-replace (list-var element)
  `(let
       ((replaced-list-var
         (assq-delete-all
          (car ',element) ,list-var)))
     (setq ,list-var
           (add-to-list 'replaced-list-var ',element))))

(defmacro duc/alist-replace-set (list-var element)
  `(setq ,list-var (duc/alist-replace ,list-var ,element)))

(defvar duc/font-family)
(setq duc/font-family "Jetbrains Mono")
(defvar duc/font-height)
(setq duc/font-height 140)
(defvar duc/font-height-mode-line)
(setq duc/font-height-mode-line 120)
(defvar duc/font-weight)
(setq duc/font-weight 'normal)
(defconst duc/font-weights (list 'ultra-bold
                                 'extra-bold
                                 'bold
                                 'semi-bold
                                 'normal
                                 'semi-light
                                 'light
                                 'extra-light
                                 'ultra-light))
(defvar duc/font-line-spacing)
(setq duc/font-line-spacing 1)
(setq-default line-spacing duc/font-line-spacing)

(defun duc/font-weight-cycle ()
  (interactive)
  (setq duc/font-weight (or (car (cdr (member duc/font-weight duc/font-weights)))
                            'ultra-bold))
  (set-face-attribute 'default nil
                      :weight duc/font-weight)
  (print duc/font-weight))

(defun duc/font-size-increase ()
  (interactive)
  (setq duc/font-height (+ duc/font-height 10))
  (set-face-attribute 'default nil
                      :height duc/font-height)
  (setq duc/font-height-mode-line
        (+ duc/font-height-mode-line 10))
  (dolist (sym '(mode-line mode-line-inactive))
    (set-face-attribute sym nil
     :height duc/font-height-mode-line))
  (print duc/font-height))

(defun duc/font-size-decrease ()
  (interactive)
  (setq duc/font-height (+ duc/font-height -10))
  (set-face-attribute 'default nil
                      :height duc/font-height)
  (setq duc/font-height-mode-line
        (+ duc/font-height-mode-line -10))
  (dolist (sym '(mode-line mode-line-inactive))
    (set-face-attribute sym nil
     :height duc/font-height-mode-line))
  (print duc/font-height))

(defun duc/set-font-size ()
  (interactive)
  (setq duc/font-height (string-to-number (completing-read "font size: "
                                         '("140"))))
  (set-face-attribute 'default nil
                      :height duc/font-height
                      :weight duc/font-weight
                      :width 'normal))

;; font chooser
(defun duc/ivy-font ()
  (interactive)
  (setq duc/font-family (completing-read "font: "
                                         (font-family-list)))
  (set-face-attribute 'default nil
                      :family duc/font-family
                      :height duc/font-height
                      :weight duc/font-weight
                      :width 'normal))

(defun duc/set-font-line-spacing ()
  (interactive)
  (setq duc/font-line-spacing (string-to-number (completing-read "line spacing: "
                                                                 '("0"))))
  (setq-default line-spacing duc/font-line-spacing))

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

(defun duc/racket-eval-last-sexp ()
  "Eval the previous sexp asynchronously and `message' the result."
  (interactive)
  (racket--cmd/async
   `(eval
     ,(buffer-substring-no-properties (duc/racket--repl-last-sexp-start)
                                      (+ (point) 1)))
   (lambda (v)
     (message "%s" v))))

(defun duc/racket--repl-last-sexp-start ()
  (save-excursion
    (condition-case ()
        (progn
          (forward-char)
          (backward-list)
          (point))
      (scan-error (user-error "There isn't a complete s-expression before point")))))

(defun duc/eval-dwim (p)
  (interactive "P")
  (pcase major-mode
    ('racket-mode (duc/racket-eval-last-sexp))
    ('emacs-lisp-mode (eval-last-sexp p))
    ('python-mode (shell-command (concat "python3" " " (buffer-name))))
    ('latex-mode (preview-section))
    (_ (eval-last-sexp p))))

(setq async-shell-command-display-buffer nil)
(setq shell-command-dont-erase-buffer 'end-last-out)

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

(defun duc/new-buffer ()
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (switch-to-buffer-other-window buffer)))

(defun duc/modeline-fontsize ()
  "Return font size of modeline."
  duc/font-height-mode-line)

(defun duc/theme-setup-modeline  (&rest _)
  "Theme modeline."
  (duc/update-modeline-variables)
  (duc/prefix-modeline)
  (let ((font-size duc/font-height))
    (dolist (sym '(mode-line mode-line-inactive))
      (let ((border-color (duc/modeline-border-color)))
        (if border-color
            (set-face-attribute
             sym
             nil
             :height font-size
             :box `(:line-width 1 :color ,border-color))
          (set-face-attribute
           sym
           nil
           :height font-size))))))

(defun duc/theme-fontsize ()
  "Return font size of modeline."
  duc/font-height)

(defun duc/modeline-fontsize ()
  "Return font size of modeline."
  duc/font-height-mode-line)

(defun duc/theme-get-variable-pitch-font ()
  "iA Writer Duospace")

(defun duc/theme-background-color (sym)
  "Return background color of modeline."
  ;; If `mode-line-inactive' doesn't specify a background, use
  ;; `mode-line''s instead.
  (let* ((frame (selected-frame))
         (background (face-attribute sym :background frame)))
    (if (and
         (eq sym 'mode-line-inactive)
         (eq background 'unspecified))
        (face-attribute 'mode-line :background frame)
      background)))

(defun duc/theme-setup-modeline (&rest _)
  "Theme modeline."
  (interactive)
  (setq underline-minimum-offset 999)
  (set-frame-parameter (selected-frame) 'right-divider-width 1)
  (unless (member '(right-divider-width . 1) default-frame-alist)
    (push '(right-divider-width . 1) default-frame-alist))
  (let* ((font (duc/theme-get-variable-pitch-font))
         (font-size (duc/modeline-fontsize))
         (border-color (face-foreground 'window-divider (selected-frame) t))
         (underline `(:color ,border-color))
         (overline border-color))
    (dolist (sym '(mode-line mode-line-inactive))
      (set-face-attribute
       sym
       nil
       :family font
       :height font-size
       :box `(:line-width 5 :color ,(duc/theme-background-color sym))
       :underline underline
       :overline overline))))

;; POST region to pastebin-like service, ix.io.
(defun duc/ixio ()
  (interactive)
  (let ((short (string-trim (shell-command-to-string
                       (format "echo %s | curl -sF 'f:1=<-' ix.io"
                               (shell-quote-argument (buffer-substring (region-beginning) (region-end))))))))
    (cond ((string-match "^http://ix.io/[a-zA-Z0-9]+$" short) (kill-new short)
                                                              (print short))
          (t (print (format "Error calling ix.io: %s" short))))))

(defun duc/create-linked-note ()
  (interactive)
  (let* ((title (completing-read "Name: " nil))
         (fname (concat "~/dev/huhmann/"
                        (format-time-string "%y%2U%2u")
                        "-"
                        (org-id-new)
                        (let ((name (replace-regexp-in-string " " "-" (downcase title))))
                          (if (not (string= "" name)) (concat "-" name)
                            ""))
                        ".org"
                        )))
    (append-to-file
     (concat "#+TAGS: \n\n" title "\n\nLinks") nil fname)
    (find-file fname)))

(defun duc/counsel-insert-linked-link-action (x)
  "Go to occurrence X in current Git repository."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let ((file-name (match-string-no-properties 1 x))
          (line-number (match-string-no-properties 2 x)))
      (goto-char (point-max))
      (insert "\n")
      (insert (concat "- " "[[" "file:" file-name "][" (completing-read "Link title: " nil) "]]"))
      (swiper--ensure-visible)
      (run-hooks 'counsel-grep-post-action-hook)
      (unless (eq ivy-exit 'done)
        (swiper--cleanup)
        (swiper--add-overlays (ivy--regex ivy-text))))))

(defun duc/counsel-ag-insert-linked-link (&optional initial-input initial-directory extra-ag-args ag-prompt)
  "Grep for a string in the current directory using ag.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS string, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument. "
  (interactive)
  (setq counsel-ag-command counsel-ag-base-command)
  (setq counsel--regex-look-around counsel--grep-tool-look-around)
  (counsel-require-program counsel-ag-command)
  (when current-prefix-arg
    (setq initial-directory
          (or initial-directory
              (read-directory-name (concat
                                    (car (split-string counsel-ag-command))
                                    " in directory: "))))
    (setq extra-ag-args
          (or extra-ag-args
              (read-from-minibuffer (format
                                     "%s args: "
                                     (car (split-string counsel-ag-command)))))))
  (setq counsel-ag-command (counsel--format-ag-command (or extra-ag-args "") "%s"))
  (let ((default-directory (or initial-directory
                               (counsel--git-root)
                               default-directory)))
    (ivy-read (or ag-prompt
                  (concat (car (split-string counsel-ag-command)) ": "))
              #'counsel-ag-function
              :initial-input initial-input
              :dynamic-collection t
              :keymap counsel-ag-map
              :history 'counsel-git-grep-history
              :action #'duc/counsel-insert-linked-link-action
              :unwind (lambda ()
                        (counsel-delete-process)
                        (swiper--cleanup))
              :caller 'counsel-ag)))

;; Use this method to query init load duration
;(emacs-init-time)
