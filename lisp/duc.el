;;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(require 'five-letter-words)

(defvar duc/font-family (pcase system-type
                          ('gnu/linux "JetBrains Mono")
                          (_ "InconsolateG for Powerline")))

(defvar duc/font-height (pcase system-type
                          ('windows-nt 100)
                          ('gnu/linux 140)
                          (_ 140)))


(defvar duc/font-family-mode-line (pcase system-type
                                    ('windows-nt "Calibri")
                                    ('gnu/linux "DejaVu Sans Mono")
                                    (_ "Concourse T3 Tab")))

(defvar duc/font-height-mode-line (pcase system-type
                          ('windows-nt 120)
                          ('gnu/linux 120)
                          (_ 160)))

(defvar duc/font-family-variable-pitch (pcase system-type
                                         ('darwin "SF Pro Text")
                                         (_ "Sans Serif")))

(defvar duc/margin-height-mode-line 1)

(defvar duc/font-weight 'normal)

(defconst duc/font-weights (list 'ultra-bold
                                 'extra-bold
                                 'bold
                                 'semi-bold
                                 'normal
                                 'semi-light
                                 'light
                                 'extra-light
                                 'ultra-light))

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
(defun duc/set-font ()
  (interactive)
  (setq duc/font-family (completing-read "font: "
                                         (font-family-list)))
  (set-face-attribute 'default nil
                      :font (find-font (font-spec :name duc/font-family))
                      :height duc/font-height
                      :weight duc/font-weight
                      :width 'unspecified)
  (set-face-attribute 'org-block nil
                      :inherit '(fixed-pitch shadow)
                      :extend t
                      :family duc/font-family))

(defun duc/set-font-line-spacing ()
  (interactive)
  (setq duc/font-line-spacing (string-to-number (completing-read "line spacing: "
                                                                 '("0"))))
  (setq-default line-spacing duc/font-line-spacing))

(defun duc/selectrum-load-theme ()
  (interactive)
  (load-theme (intern
               (completing-read "Load custom theme: "
                                (mapcar 'symbol-name
                                        (custom-available-themes))
                                nil
                                t))
              t))

(defun duc/ivy-terminal ()
  (interactive)
  (let ((terminal-buffers (seq-filter (lambda (x)
                                        (string-match-p
                                         (regexp-quote "terminal-") x))
                                      (mapcar (function buffer-name) (buffer-list)))))
    (let* ((initial-buffer-name
            (concat "terminal-"
                    (nth (random (length duc/five-letter-verbs)) duc/five-letter-verbs)
                    "-"
                    (nth (random (length duc/five-letter-nouns)) duc/five-letter-nouns)))
           (buffer-name (completing-read "shell : " terminal-buffers nil nil initial-buffer-name)))
      (if (member buffer-name terminal-buffers)
          (switch-to-buffer buffer-name)
        (vterm (concat buffer-name))))))

(defun duc/run-this-in-eshell (cmd)
  "Runs the command 'cmd' in eshell."
  (with-current-buffer "*eshell*"
    (let ((toggle-readonly (eq evil-state 'normal)))
      (when toggle-readonly
        (evil-insert 1))
      (eshell-kill-input)
      (end-of-buffer)
      (insert cmd)
      (eshell-send-input)
      (when toggle-readonly
        (evil-normal-state))
      (set-window-point (get-buffer-window "*eshell*") (point-max)))))

(defun duc/ivy-shell-send-string (string &optional terminal working-directory clear)
  (let ((current-buffer-p (current-buffer))
        (candidate-terminal-buffers (mapcar (function buffer-name) (buffer-list))))
    (let ((buffer-name (if terminal
                           terminal
                         (completing-read "shell : " candidate-terminal-buffers))))
     (if (member buffer-name candidate-terminal-buffers)
         (pop-to-buffer buffer-name)
       (vterm buffer-name))
     (when working-directory
       (vterm-send-string (concat "cd " working-directory))
       (vterm-send-return))
     (when clear
       (vterm-clear))
     (vterm-send-string string)
     (vterm-send-return)
     (pop-to-buffer current-buffer-p))))

(defun duc/completing-shell-history ()
  (interactive)
  (let ((history
         (split-string
          (replace-regexp-in-string "^: [0-9]+:0;" ""
                                    (with-temp-buffer
                                      (insert-file-contents (or local/shell-history-file
                                                                "~/.zsh_history"))
                                      (buffer-substring-no-properties
                                       (point-min)
                                       (point-max))))
          "\n" t)))
    (completing-read "$ " history)))

(defun duc/shell-send-string-to-project-dwim (&optional command working-directory)
  (interactive)
  (let ((working-directory
         (or working-directory
             (if (projectile-project-p)
                 (projectile-acquire-root)
               (file-name-directory (or (buffer-file-name)
                                        (project-find-file))))))
        (command (or command (duc/completing-shell-history))))
    (let ((directory-name (car (last (split-string working-directory "/" t "") 1))))
      (setq my-working-directory working-directory)
      (if (s-starts-with-p "/ssh:" working-directory)
          ; Is tramp path eg, /ssh:root@143.198.51.22:/
          (duc/run-this-in-eshell command) ; HACK - Assumes we already got tramp session setup.
                                           ;        Ignores value of working-directory.
        ; Not tramp path, send to local terminal
        (duc/ivy-shell-send-string command
                                   (concat "terminal-" directory-name)
                                   working-directory))
      (message (concat "terminal-" directory-name " --> " working-directory " - " command)))))

(defun duc/sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

;; https://blog.00null.net/post/145106940296/use-the-unix-generating-a-random-password
(defun duc/generate-password ()
  (interactive)
  (let ((password (string-trim (shell-command-to-string "head -c 16 /dev/random | base64 | tr -d '=' | tr '+/' '-_'"))))
    (message password)
    password))

(defun duc/seq-random-choose (sequence &optional limit)
  (let ((n (min (or limit (seq-length sequence))
                (seq-length sequence)))
        (s (copy-sequence sequence)))
    (dotimes (i n)
      (let* ((p (+ i (random (- (seq-length s) i))))
             (ival (seq-elt s i))
             (pval (seq-elt s p)))
        (setf (seq-elt s p) ival)
        (setf (seq-elt s i) pval)))
    (seq-take s n)))

;; e.g. [[file:~/../../WebAuthActivity.kt::250]]
(defun duc/org-link-create-filename-line-number ()
  (interactive)
  (concat "["
          "[" "file:" buffer-file-truename "::" (number-to-string (line-number-at-pos)) "]"
          "[" (string-trim (shell-command-to-string "git rev-parse --short HEAD")) "]"
          "]"))

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

(defun duc/scheme-send-last-sexp ()
  (interactive)
  (scheme-send-region (duc/scheme--repl-last-sexp-start)
                      (+ (point) 1)))

(defun duc/scheme--repl-last-sexp-start ()
  (save-excursion
    (forward-char)
    (backward-list)
    (point)))

"""
This function can be used instead of geiser-eval-last-sexp.
The difference is that this function will also display
last-sexp in the inferior process.
e.g.
1 (user) => (define (compose f g)
              (lambda args
                (f (apply g args))))

;Value: compose
"""
(defun duc/geiser-eval-last-sexp ()
  (interactive "P")
  (let* ((default-indent-level 9)
         (repl-prompt "> ")
         bosexp
         (eosexp (save-excursion (backward-sexp)
                                 (setq bosexp (point))
                                 (forward-sexp)
                                 (point)))
         (expression (buffer-substring bosexp eosexp))
         ; Figure out the indent-level to use for the input expr
         (indent-level (+ (string-width repl-prompt)
                          (or (with-current-buffer "* Mit REPL *"
                                (save-excursion
                                  (goto-char (point-max))
                                  (if (eq (point-at-bol) (point-at-eol))
                                      (goto-char (- (point-max) 1)))
                                  (string-match-p (concat repl-prompt "$") (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
                              default-indent-level))))
    (with-temp-buffer
      (let ((new-expression (replace-regexp-in-string "\n" (concat "\n" (make-string indent-level ? )) expression)))
        (insert new-expression)
        (message (concat ">" new-expression)))
      (message (buffer-substring (point-min) (point-max)))
      (append-to-buffer "* Mit REPL *" (point-min) (point-max))))
  (with-current-buffer "* Mit REPL *"
    (geiser-repl--maybe-send)))

(defun duc/eval-dwim-org-latex-fragment ()
  (interactive)
  (if (org-inside-LaTeX-fragment-p)
      (org--latex-preview-region (point-min) (point-max))
    (if (use-region-p)
        (org-clear-latex-preview (region-beginning) (region-end))
      (org-clear-latex-preview
       (if (org-before-first-heading-p) (point-min)
         (save-excursion
           (org-with-limited-levels (org-back-to-heading t) (point))))
       (org-with-limited-levels (org-entry-end-position))))))

(defun duc/eval-dwim (p)
  (interactive "P")
  (pcase major-mode
    ('racket-mode (duc/racket-eval-last-sexp))
    ('scheme-mode (if (bound-and-true-p geiser-mode)
                      (duc/geiser-eval-last-sexp)
                    (duc/scheme-send-last-sexp)))
    ('emacs-lisp-mode (eval-last-sexp p))
    ('python-mode
     (cond ((string-match-p ".*\\/EPIJudge\\/.*" (or (buffer-file-name) ""))
            (let ((terminal "terminal-epijudge"))
              (duc/ivy-shell-send-string (concat "python " (buffer-name))
                                         terminal
                                         (file-name-directory (buffer-file-name)))
              (display-buffer terminal)))
           (t
            (unless (get-buffer (format "*Python[%s]*" (buffer-name)))
              (let ((buffer (buffer-name)))
                (run-python nil t t)
                (pop-to-buffer buffer)))
            (cond ((use-region-p) (python-shell-send-string
                                   (buffer-substring (region-beginning)
                                                     (region-end))))
                  (t (python-shell-send-buffer))))))
    ('latex-mode (preview-section))
    ('org-mode (duc/eval-dwim-org p))
    (_ (eval-last-sexp p))))

(defun duc/org-src-block-parameter-property (parameter params-as-string)
  (if (stringp params-as-string)
      (let ((parameters (mapcar
                         (lambda (token) (if (string-prefix-p ":" token) (intern token) token))
                         (split-string params-as-string " " t " "))))
        (plist-get parameters parameter))))

;(src-block (:language "emacs-lisp"
;            :switches nil
;            :parameters :dir "~/"
;            :begin 135 :end 197
;            :number-lines nil :preserve-indent nil
;            :retain-labels t :use-labels t
;            :label-fmt nil
;            :value "    (setq foo 'bar)"
;            :post-blank 3
;            :post-affiliated 135
;            :parent nil))
(defun duc/eval-dwim-org (p)
  (interactive "P")
  (cond ((org-in-src-block-p t)
         (let ((lang (org-element-property :language (org-element-at-point)))
               (dir (duc/org-src-block-parameter-property
                     :dir
                     (org-element-property :parameters (org-element-at-point)))))
           (cond ((string-equal lang "emacs-lisp")
                  (eval-last-sexp p))
                 ((string-equal lang "bash")
                  (let ((line-of-bash (string-trim (buffer-substring (line-beginning-position)
                                                                     (line-end-position)))))
                    (duc/shell-send-string-to-project-dwim line-of-bash dir))))))
        (t (duc/eval-dwim-org-latex-fragment))))

(defun duc/eval-print-dwim (p)
  (interactive "P")
  (pcase major-mode
    ('scheme-mode (if (bound-and-true-p geiser-mode)
                      (let ((geiser-mode-eval-last-sexp-to-buffer t)
                            (geiser-mode-eval-to-buffer-prefix "\n;; "))
                        (geiser-eval-last-sexp p))
                    (duc/scheme-send-last-sexp)))
    ('emacs-lisp-mode (let ((eval-expression-print-length 1000)
                            (eval-expression-print-level 100))
                        (eval-print-last-sexp p)))
    (_ (let ((eval-expression-print-length 1000)
             (eval-expression-print-level 100))
         (eval-print-last-sexp p)))))

(setq async-shell-command-display-buffer nil)
(setq shell-command-dont-erase-buffer 'end-last-out)

(setq python-shell-interpreter "python3")
(setq python-shell-completion-native-enable nil)

(defun duc/eval-buffer ()
  (interactive)
  (pcase major-mode
    ('racket-mode (racket-run))
    ('emacs-lisp-mode (eval-buffer))
    (_ (eval-buffer))))

(defun duc/pretty-print-dwim ()
  (interactive)
  (pcase major-mode
    ('javascript-mode (json-pretty-print))
    ('emacs-lisp-mode (indent-pp-sexp t))
    (_ (indent-pp-sexp t))))

(defvar-local duc/header-line-format nil)

(defun duc/mode-line-in-header ()
  "Toggle displaying mode-line in header instead of footer
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
  (let* ((initial-buffer-name
          (concat
           "buffer-"
           (nth (random (length duc/five-letter-verbs)) duc/five-letter-verbs)
           "-"
           (nth (random (length duc/five-letter-nouns)) duc/five-letter-nouns)))
         (buffer
         (generate-new-buffer
          (completing-read "New buffer name: "
                           nil
                           nil
                           nil
                           initial-buffer-name))))
    (set-buffer-major-mode buffer)
    (split-window)
    (switch-to-buffer buffer)))

;; Credit [Pin an buffer to a window in #emacs](https://gist.github.com/HeinrichHartmann/c4401ff0347cea975380e221c7e24f42).
(defun duc/toggle-pin-buffer ()
  "Pin buffer to current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "pinned buffer" "un-pinned buffer")))

(defun duc/theme-background-color (sym)
  "Return background color of mode-line."
  ;; If `mode-line-inactive' doesn't specify a background, use
  ;; `mode-line''s instead.
  (let* ((frame (selected-frame))
         (background (face-attribute sym :background frame)))
    (if (and
         (eq sym 'mode-line-inactive)
         (eq background 'unspecified))
        (face-attribute 'mode-line :background frame)
      background)))

(defun duc/theme-setup-mode-line (&rest _)
  "Theme mode-line."
  (interactive)
  (setq underline-minimum-offset 999)
  (set-frame-parameter (selected-frame) 'right-divider-width 1)
  (unless (member '(right-divider-width . 1) default-frame-alist)
    (push '(right-divider-width . 1) default-frame-alist))
  (let* ((font duc/font-family-mode-line)
         (font-size duc/font-height-mode-line)
         (border-color (face-foreground 'window-divider (selected-frame) t))
         (underline `(:color ,border-color))
         (overline border-color))
    (dolist (sym '(mode-line mode-line-inactive))
      (set-face-attribute
       sym
       nil
       :family font
       :height font-size
       :box `(:line-width ,duc/margin-height-mode-line :color ,(duc/theme-background-color sym))
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

(defun duc/delete-this-file ()
  """
Delete file for current file buffer. Does not prompt.

Author: @syegge.
"""
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (file-exists-p filename)
        (progn
          (delete-file filename)
          (message "Deleted %s" filename))
      (message "This buffer is not visiting an existeing file."))))

(defun duc/git-clone ()
  (interactive)
  (eval
   '(let* ((repository-url (completing-read "Repository url: " nil))
          (repository-directory (substring (car (last (split-string repository-url "/"))) 0 -4))
          (parent-directory (read-file-name "Clone to parent directory: " "~/"))
          (clone-directory (concat parent-directory repository-directory))
          ;; [SO: Run elisp when `async-shell-command` is done](https://emacs.stackexchange.com/a/42174).
          (output-buffer (generate-new-buffer "*git clone status*"))
          (proc (progn
                  (async-shell-command (format "git clone %s %s"
                                               repository-url
                                               clone-directory)
                                       output-buffer)
                  (get-buffer-process output-buffer))))
     (if (process-live-p proc)
         (set-process-sentinel proc
                               #'(lambda (process signal)
                                   (when (memq (process-status process) '(exit signal))
                                     (message "git clone status: success")
                                     (magit-status clone-directory)
                                     (shell-command-sentinel process signal))))
       (message "git clone status: no process")))
   t))

(defcustom duc/create-linked-note-default-dir "~/dev/huhmann/" nil)
(defcustom duc/create-bnote-default-dir "~/dev/notes/" nil)
(defcustom duc/bnote-template-default-dir "~/dev/notes/templates" nil)

(defun duc/create-linked-note (&optional project)
  (interactive)
  (let* ((project (if project project duc/create-linked-note-default-dir))
         (title (completing-read (concat "[" project "] " "Name: ") nil))
         (fname (concat project
                        (format-time-string "%y%2U%2u")
                        "-"
                        (org-id-new)
                        (let ((name (replace-regexp-in-string " " "-" (downcase title))))
                          (if (not (string= "" name)) (concat "-" name)
                            ""))
                        ".org"
                        )))
    (append-to-file
     (concat "#+TAGS: \n\n* " title "\n\n* Links") nil fname)
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
  (let ((counsel--regex-look-around counsel--grep-tool-look-around))
    (let ((counsel-ag-command counsel-ag-base-command))
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
                                         (car (split-string counsel-ag-command))))))))
    (let ((counsel-ag-command (counsel--format-ag-command (or extra-ag-args "") "%s"))
          (default-directory (or initial-directory
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
                :caller 'counsel-ag))))

(defun duc/completing-bnote-insert-linked-link ()
  (interactive)
  (let ((filename (completing-read
               "insert bnote link: "
               (directory-files duc/create-bnote-default-dir nil "\.org$"))))
    (insert
     (format "[[-:%s]]" (file-name-sans-extension filename)))))

(defconst duc/bnote-default-template
  "#+STARTUP: showeverything indent


* Log


* Backlinks")

(defconst duc/bnote-study-template
  "#+STARTUP: showall indent


* Mind dump
:PROPERTIES:
:VISIBILITY: folded
:END:


* Thinking about the subject
:PROPERTIES:
:VISIBILITY: folded
:END:


* Questions


* Summary (3-5 sentences)
:PROPERTIES:
:VISIBILITY: folded
:END:


- What are the key ideas?

- How can I apply this knowledge that I learned?

- How do these ideas relate to what I already know?


* Backlinks")

(defun duc/create-or-open-bnote-type (type &optional template)
  (interactive)
  (let* ((new-entry-name (concat type
                                 "-"
                                 (format-time-string "%y%2m%2d")
                                 ".org"))
         (new-entry-file (concat duc/create-bnote-default-dir
                                 new-entry-name))
         (entry-exists (or (get-buffer new-entry-name)
                           (file-exists-p new-entry-file)))
         (force-template (if template t))
         (template (or template duc/bnote-default-template)))
    (unless entry-exists
                                        ; Entry not found, so we'll create a new entry.
      (if force-template
          (append-to-file template nil new-entry-file)
        (let ((last-entry (car (last (directory-files duc/create-bnote-default-dir
                                                      t
                                                      (concat "^" type "-"))))))
          (if last-entry
                                        ; Copy over the most recent entry if exists.
              (let ((new-entry-file-buffer (find-file-noselect new-entry-file)))
                (with-current-buffer new-entry-file-buffer
                  (insert-file-contents last-entry)
                                        ; Reload major-mode incl hooks.
                  (normal-mode)))
                                        ; Else copy over the template.
            (append-to-file template nil new-entry-file)))))
    (find-file new-entry-file)
    (unless entry-exists
      (search-forward "*"))))

(defun duc/completing-bnote-type ()
  (interactive)
  (let* ((type (completing-read "type: " '("bnote" "morning" "evening" "study")))
         (template (and (string-prefix-p "study" type)
                        duc/bnote-study-template)))
    (duc/create-or-open-bnote-type type template)))

(defun duc/completing-bnote-template ()
  (interactive)
  (let* ((files (directory-files duc/bnote-template-default-dir nil "\.org$"))
         (file (completing-read "template: " files nil t))
         (type (file-name-sans-extension file))
         (template (with-temp-buffer
                     (insert-file-contents (concat duc/bnote-template-default-dir
                                                   "/"
                                                   file))
                     (buffer-substring-no-properties (point-min)
                                                     (point-max)))))
    (duc/create-or-open-bnote-type type template)))

(defun duc/insert-bnote-lozenge-empty-link ()
    (interactive)
  (insert (concat "[[◊:" (format-time-string "%y%2m%2d") "]]")))

(defun duc/add-bnote-with-char (bullet)
  (interactive)
  (let ((indent-level
         (or (string-match-p "[•◦◼◻<>-]"
                             (buffer-substring (line-beginning-position)
                                               (line-end-position)))
             2)))
    (move-end-of-line nil)
    (open-line 1)
    (next-line)
    (insert
     (concat (make-string indent-level ? )
             bullet
             " "))
    (move-end-of-line nil)))

(defun bnote-auto-fill-function ()
  ;; Replaces org-auto-fill-function as fn for normal-auto-fill-function
  ;; under auto-fill-mode.
  ;; Set this function within org file local variables:
  ;; # Local Variables:
  ;; # normal-auto-fill-function: bnote-auto-fill-function
  ;; # End:
  ;;
  ;; Check if auto-filling is meaningful.
  (let ((adaptive-fill-regexp (purecopy "[ \t]*\\([-–!|#%;>*·•◼‣⁃◦◻]+[ \t]*\\)*"))
        (fc (current-fill-column)))
    (when (and fc (> (current-column) fc))
      (let* ((fill-prefix
              (replace-regexp-in-string "[•◦◼◻<>-]"
                                        " "
                                        (let ((line-sans-bnote-prefixes (replace-regexp-in-string "^[\\^x!]"
                                                                                                  " "
                                                                                                  (thing-at-point 'line t))))
                                          (if (string-match adaptive-fill-regexp
                                                            line-sans-bnote-prefixes)
                                              (match-string 0 line-sans-bnote-prefixes)))))
             ;; Enforce empty fill prefix, if required.  Otherwise, it
             ;; will be computed again.
             (adaptive-fill-mode (not (equal fill-prefix ""))))
        (when fill-prefix (do-auto-fill))))))

(defun duc/bnote-update-backlinks-for-note ()
  (let ((back-buffer (current-buffer))
        (back-filename (buffer-file-name (current-buffer)))
        (files-to-update
         (seq-filter (lambda (f) (or (get-file-buffer f)
                                     (file-exists-p f)))
                     (org-element-map (org-element-parse-buffer) 'link
                       (lambda (link)
                         (when (and (string= (org-element-property :type link) "file")
                                    (not (string= (org-element-property
                                                   :raw-value (org-element-property
                                                               :parent (org-element-property :parent link)))
                                                  "Backlinks"))
                                    (s-ends-with-p ".org" (org-element-property :path link)))
                           (org-element-property :path link)))))))
    ; Update backlinks for each file.
    (mapcar
     (lambda (f)
       (let ((forward-buffer (find-file-noselect f)))
         (with-current-buffer forward-buffer
           ; When note file contains a backlinks section..
           (when (org-element-map (org-element-parse-buffer) 'headline
                   (lambda (h1)
                     (and (= (org-element-property :level h1) 1)
                          (string= (org-element-property :raw-value h1) "Backlinks"))))
             ; ..build list of existing backlinks..
             (let ((existing-backlinks-in-f
                    (org-element-map (org-element-parse-buffer) 'link
                      (lambda (link)
                        (when (and (string= (org-element-property :type link) "file")
                                   (string= (org-element-property :raw-value
                                                                  (org-element-property
                                                                   :parent (org-element-property
                                                                            :parent link)))
                                            "Backlinks"))
                          (org-element-property :path link))))))
               ; ..and add backlink if new entry.
               (when (not (member (file-name-nondirectory back-filename) existing-backlinks-in-f))
                 (with-current-buffer forward-buffer
                   (goto-char (point-max))
                   (insert (format "** [[-:%s]].\n" (file-name-base back-filename))))))))))
     files-to-update))
  ; Return NIL to open link.
  nil)

(defun duc/org-open-at-point-follow-with-template ()
  (let* ((current-element (org-element-context))
         (f (when (and (eq 'link (car current-element))
                       (string= "file" (org-element-property :type current-element))
                       (s-ends-with-p ".org"
                                      (org-element-property :path current-element)))
                 (org-element-property :path current-element))))
    (when (and f (not (or (get-file-buffer f)
                          (file-exists-p f))))
      (let ((buf (find-file-noselect f)))
        (with-current-buffer buf
          (let ((title (capitalize
                        (string-replace
                         "-"
                         " "
                         (file-name-sans-extension f)))))
            (insert (format "#+STARTUP: showall indent
#+TITLE: %s
#+DATE: %s
#+TAGS:
#+LINK: %s

* %s



* Backlinks\n" title (current-time-string) "- file:%s.org" title)))
          ; Reload in-buffer settings.
          (org-mode-restart)))))
  ; Return NIL to open link.
  nil)

(add-hook 'org-open-at-point-functions #'duc/bnote-update-backlinks-for-note)
(add-hook 'org-open-at-point-functions #'duc/org-open-at-point-follow-with-template)

(defun duc/bnote-convert-str-to-link ()
  "Replaces STR in region with its org-link
Intended for use in bnotes.
e.g., Hello World -> [[-:hello-world]]"
  (interactive)
  (when (use-region-p)
    (let* ((str (buffer-substring (region-beginning)
                                  (region-end)))
           ; Hard-coding assumption
           ; #+LINK: - file:%s.org
           (link (format "[[-:%s]]" (downcase (string-replace " " "-" str)))))
      (replace-string-in-region str link (region-beginning) (region-end)))))

(defun duc/anki-connect-push ()
  (interactive)
  (let ()
    ;; enable anki-editor-mode if it isn't. Enabling the mode will enable uploading local media to anki.
    (if (not (bound-and-true-p anki-editor-mode))
        (anki-editor-mode))
    (anki-editor-push-notes)))

(defun duc/play-sound (&optional filepath)
  (interactive)
  (let* ((filepath (shell-quote-argument ; Escape special characters to be
                                        ; compatible with words like
                                        ; 'Raison d’être'.
                   (expand-file-name    ; Resolve path so that tilde char
                                        ; won't be escaped and make the
                                        ; path invalid.
                    (or filepath (completing-read "sound file to play: ")))))
         (ext (file-name-extension filepath)))
    (cond ((string-match-p ext "mp3|wav")
           ; afplay included with macOS
           (shell-command-to-string (format "afplay %s" filepath)))
          ((string-match-p ext "ogg")
           ; ogg123 included with vorbis-tools
           (shell-command-to-string (format "ogg123 --quiet %s" filepath)))
          (t (error (format "Unsupported extension for file %s" filepath))))))

(defun duc/play-sounds (playlist)
  (dolist (filepath playlist)
    (duc/play-sound filepath)))

(defvar-local duc/sounds-in-random-order--playlist nil)

(defun duc/sounds-in-random-order-play (filelist-a filelist-b &optional limit-a limit-b)
  (setf duc/sounds-in-random-order--playlist
        (duc/seq-random-choose
         (seq-concatenate 'list
                          (duc/seq-random-choose filelist-a (or limit-a 1))
                          (duc/seq-random-choose filelist-b (or limit-b 1)))))
  (duc/sounds-in-random-order-replay))

(defun duc/sounds-in-random-order-replay ()
  (dolist (filepath duc/sounds-in-random-order--playlist)
    (duc/play-sound filepath)))

(defun duc/sounds-in-random-order-playlist ()
  (mapcar (lambda (filepath)
            (let ((name (file-name-base filepath)))
              (cond ((string-match "\\(forvo-vi\\)-\\(.+\\)-\\([0-9]+\\)"
                                   name)
                     (match-string 2 name))
                    (t name))))
          duc/sounds-in-random-order--playlist))

;; soundoftext.com

(defun duc/sot-generate-sound--action (text voice)
  (let (a)
    (push `(engine . ,"Google") a)
    (push `(data . ,(let (data)
                      (push `(text . ,text) data)
                      (push `(voice . ,voice) data))) a)))

(defun duc/sot-generate-sound--post (text)
  (let ((request-body (json-encode (duc/sot-generate-sound--action text "en-US")))
        (request-backend 'curl)
        (json-array-type 'list)
        reply
        err)
    (let ((response (request (format "https://api.soundoftext.com/sounds")
                      :type "POST"
                      :parser 'json-read
                      :data request-body
                      :headers '(("Content-Type" . "application/json"))
                      :success (cl-function (lambda (&key data &allow-other-keys)
                                              (setq reply data)))
                      :error (cl-function (lambda (&key _ &key error-thrown &allow-other-keys)
                                            (setq err (string-trim (cdr error-thrown)))))
                      :sync t)))
      (unless (request-response-done-p response)
        (request--curl-callback (get-buffer-process (request-response--buffer response)) "finished\n")))
    (when err (error "Error with server %s" err))
    (or reply (error "empty reply"))))

(defun duc/sot-sound-status--get (id)
  (let ((request-backend 'curl)
        (json-array-type 'list)
        reply
        err)
    (let ((response (request (format "https://api.soundoftext.com/sounds/%s" id)
                      :type "GET"
                      :parser 'json-read
                      :headers '(("Content-Type" . "application/json"))
                      :success (cl-function (lambda (&key data &allow-other-keys)
                                              (setq reply data)))
                      :error (cl-function (lambda (&key _ &key error-thrown &allow-other-keys)
                                            (setq err (string-trim (cdr error-thrown)))))
                      :sync t)))
      (unless (request-response-done-p response)
        (request--curl-callback (get-buffer-process (request-response--buffer response)) "finished\n")))
    (when err (error "Error with server %s" err))
    (or reply (error "empty reply"))))

;; e.g. url "https://soundoftext.nyc3.digitaloceanspaces.com/ce916bf0-c882-11e7-9df0-2f554923557b.mp3"
;; e.g. url "https://apifree.forvo.com/audio/3m3k1p2d1g3i1o2n2h3f1o26322l1i3p1o1g232p2h..."
(defun duc/download-mp3 (url filepath)
  (condition-case nil
      (url-copy-file url filepath)
    (file-already-exists
     (message (format "file already exists %s" filepath))))
  (kill-new filepath))

(defun duc/sot-text-to-sound-at-region ()
  (interactive)
  (let ((text (buffer-substring (region-beginning) (region-end))))
    (let ((result (duc/sot-generate-sound--post text)))
      (let ((id (cdr (assoc 'id result))))
        (message (format "<%s> copied to kill-ring %s" text id))
        (kill-new id)))))

(defun duc/sot-text-to-sound-download-if-ready ()
  (interactive)
  (let ((id (completing-read "[Sound of Text] status for: "
                             (cons (current-kill 0 t) kill-ring))))
    (let ((result (duc/sot-sound-status--get id)))
      (cond ((string= "Done" (cdr (assoc 'status result)))
             (duc/download-mp3 (cdr (assoc 'location result)) (concat "~/dev/notes/ttv/" id ".mp3")))
            ((string= "Error" (cdr (assoc 'status result)))
             (message (format "error occurred for download %s" (cdr (assoc 'message result)))))
            (message "not ready for download")))))

;; forvo.com
;; https://api.forvo.com/documentation/word-pronunciations/

(defun duc/forvo-query-for-mp3--get (word)
  (let ((limit 5)
        (apikey local/forvo-api-key)
        (request-backend 'curl)
        (json-array-type 'list)
        reply
        err)
                               ; e.g. https://apifree.forvo.com/action/word-pronunciations/format/json/word/forvo/id_lang_speak/39/order/rate-desc/limit/1/key/XXXX/
    (let ((response (request (format "https://apifree.forvo.com/action/word-pronunciations/format/json/word/%s/language/vi/order/rate-desc/limit/%s/key/%s/"
                                     word
                                     limit
                                     apikey)
                      :type "GET"
                      :parser 'json-read
                      :headers '(("Content-Type" . "application/json"))
                      :success (cl-function (lambda (&key data &allow-other-keys)
                                              (setq reply data)))
                      :error (cl-function (lambda (&key _ &key error-thrown &allow-other-keys)
                                            (setq err (string-trim (cdr error-thrown)))))
                      :sync t)))
      (unless (request-response-done-p response)
        (request--curl-callback (get-buffer-process (request-response--buffer response)) "finished\n")))
    (when err (error "Error with server %s" err))
    (or reply (error "empty reply"))))

(defun duc/forvo-text-to-sound-at-region-or-word ()
  (interactive)
  (let* ((text (downcase (if (use-region-p)
                             (buffer-substring-no-properties (region-beginning)
                                                             (region-end))
                           (thing-at-point 'word))))
         (filepath (concat "~/dev/notes/ttv/" (format "forvo.com-%s.mp3" text))))
    (let* ((result (duc/forvo-query-for-mp3--get text))
           (item (car (cdr (assoc 'items result))))
           (pathmp3 (cdr (assoc 'pathmp3 item))))
      (message (format "<%s> copied to kill-ring %s" text pathmp3))
      (duc/download-mp3 pathmp3 filepath)
      (duc/play-sound filepath)
      (kill-new filepath))))

(defun duc/forvo-search-for-word--get (search-word apikey search-language search-limit)
  (let ((request-backend 'curl)
        (json-array-type 'list)
        reply
        err)
    (let ((response (request (format "https://apifree.forvo.com/action/word-pronunciations/format/json/word/%s/language/%s/order/rate-desc/limit/%s/key/%s/"
                                     search-word
                                     search-language
                                     search-limit
                                     apikey)
                      :type "GET"
                      :parser 'json-read
                      :headers '(("Content-Type" . "application/json"))
                      :success (cl-function (lambda (&key data &allow-other-keys)
                                              (setq reply data)))
                      :error (cl-function (lambda (&key _ &key error-thrown &allow-other-keys)
                                            (setq err (string-trim (cdr error-thrown)))))
                      :sync t)))
      (unless (request-response-done-p response)
        (request--curl-callback (get-buffer-process (request-response--buffer response)) "finished\n")))
    (when err (error "Error with server %s" err))
    (or reply (error "empty reply"))))

(defun duc/forvo-search-and-download-all (search-word &optional search-language)
  (let* ((apikey local/forvo-api-key)
         (search-language (or search-language "vi"))
         (search-limit 20)
         (download-limit 5)
         (download-dir local/forvo-download-directory)
         ; Fetch results from forvo.com
         (results (duc/forvo-search-for-word--get search-word apikey search-language search-limit))
         (items (cdr (assoc 'items results)))
         ; Forvo search is a bit loose, so we need to filter
         ; out items that don't exactly match our words.
         (items-with-my-word (seq-take
                              (seq-filter (lambda (item)
                                            (let ((actual (cdr (assoc 'word item))))
                                              (string= search-word actual)))
                                          items)
                              download-limit)))
    ; Download my words
    (mapcar (lambda (item)
              (let ((id (cdr (assoc 'id item)))
                    (word (cdr (assoc 'word item)))
                    (pathogg (cdr (assoc 'pathogg item))))
                                        ; Download it
                (let ((url pathogg)
                      (filepath (format "%s/forvo-vi-%s-%s.ogg" download-dir word id)))
                  (duc/download-mp3 url filepath))))
            items-with-my-word)))

;; Taken from spacemacs/rename-file.
(defun duc/rename-file (filename &optional new-filename)
  "Rename FILENAME to NEW-FILENAME.
When NEW-FILENAME is not specified, asks user for a new name.
Also renames associated buffer (if any exists), invalidates
projectile cache when it's possible and update recentf list."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let* ((buffer (find-buffer-visiting filename))
           (short-name (file-name-nondirectory filename))
           (new-name (if new-filename new-filename
                       (read-file-name
                        (format "Rename %s to: " short-name)))))
      (cond ((get-buffer new-name)
             (error "A buffer named '%s' already exists!" new-name))
            (t
             (let ((dir (file-name-directory new-name)))
               (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                 (make-directory dir t)))
             (rename-file filename new-name 1)
             (when buffer
               (kill-buffer buffer)
               (find-file new-name))
             (when (fboundp 'recentf-add-file)
               (recentf-add-file new-name)
               (recentf-remove-if-non-kept filename))
             (when (and (featurep 'projectile)
                        (projectile-project-p))
               (call-interactively #'projectile-invalidate-cache))
             (message "File '%s' successfully renamed to '%s'" short-name (file-name-nondirectory new-name)))))))

;; Use this method to query init load duration
;(emacs-init-time)

;; WIP
(defun sacha/fill-string (string new-fill-column &optional replace-char)
  "Wrap STRING to NEW-FILL-COLUMN. Change newlines to REPLACE-CHAR."
  (with-temp-buffer
    (insert string)
    (let ((fill-column new-fill-column))
      (fill-region (point-min) (point-max))
      (if replace-char
          (progn
            (goto-char (point-min))
            (while (re-search-forward "\n" nil t)
              (replace-match replace-char t t))))
      (buffer-string))))
(defun duc/map-to-graphviz-dot (map fill-column)
  "Convert MAP to a graphviz representation. Wrap titles at FILL-COLUMN."
  (concat
   "digraph G {\n"
   "node [shape=box,fontname=\"JetBrains Mono\",pad=1]\n"
   "edge [color=\"#CCCCCC\"]\n"
   (mapconcat
    (lambda (x)
      (format "\"%s\" -> \"%s\""
              (sacha/fill-string (car x) fill-column "\\n")
              (sacha/fill-string (cdr x) fill-column "\\n")))
    (cdr (assoc 'edges map))
    "\n")
   "\n"
   (mapconcat (lambda (x)
                (format
                 (if (null (elt x 2))
                     (concat "\"%s\" [style=filled, URL=\"#%s\", tooltip=\"%s\"]")
                   "\"%s\" [URL=\"#%s\", tooltip=\"%s\"]")
                 (sacha/fill-string (elt x 4) fill-column "\\n")
                 (replace-regexp-in-string "[^A-Za-z0-9]" "_" (elt x 4))
                 (elt x 4)))
              (cdr (assoc 'nodes map)) "\n")
   "}\n"))
(defun duc/map-to-graphviz-dot-execute ()
  (duc/map-to-graphviz-dot
   (list (cons 'nodes
               '(("A0" "B0" "C0" "D0" "A") ("A0" "B1" "C1" "D1" "B") ("A2" "B2" "C2" "D2" "C")))
         (cons 'edges
               '(("A" . "C") ("A" . "B"))))
   fill-column)
  )

(defun duc/incremental-search-filenames-in-directory ()
  (interactive)
  (let ((currentenv (getenv "FZF_DEFAULT_COMMAND")))
    (ignore-errors
      (setenv "FZF_DEFAULT_COMMAND"
              "")
      (counsel-fzf))
    (setenv "FZF_DEFAULT_COMMAND" (if (currentenv) currentenv ""))))

(defun duc/incremental-search-filenames-in-version-control ()
  (interactive)
  (let ((currentenv (getenv "FZF_DEFAULT_COMMAND")))
    (ignore-errors
      (setenv "FZF_DEFAULT_COMMAND"
              "(git ls-files --exclude-standard --others --cached ||
               ind . -maxdepth 9 -path \"*/\\.*\" -prune -o -print -o -type l -print |
               sed s/^..//) 2> /dev/null")
      (counsel-fzf))
    setenv "FZF_DEFAULT_COMMAND" (if (currentenv) currentenv "")))

(defun duc/incremental-search-filenames-dwim ()
  (interactive)
  (let* ((working-directory (if (projectile-project-p)
                                (projectile-acquire-root)
                              (pwd)))
         (active-git-project-p (projectile-file-exists-p (expand-file-name ".git" working-directory))))
    (if active-git-project-p
        (duc/incremental-search-filenames-in-version-control)
      (duc/incremental-search-filenames-in-directory))))

; Force dynamic binding on the following variables, so they get picked up by
; org-capture template.
; See [Manual: Defining Global Variables](https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Variables.html).
(defvar duc/org-code-block-filename)
(defvar duc/org-code-block-link)
(defvar duc/org-code-block-language)

(defun duc/org-capture-region-with-code-block ()
  (interactive)
  (let ((duc/org-code-block-filename (format "%s::%d" (buffer-name) (line-number-at-pos)))
        (duc/org-code-block-link (duc/org-link-create-filename-line-number))
        (duc/org-code-block-language (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))
    (org-capture nil "r")))

; https://caiorss.github.io/Emacs-Elisp-Programming/Elisp_Snippets.html
(defun duc/yank-string (s)
  "Copy a string to clipboard"
   (with-temp-buffer
    (insert s)
    (clipboard-kill-region (point-min) (point-max))))

(defun duc/yank-buffer-name ()
  (interactive)
  (duc/yank-string (buffer-name)))

(defun duc/yank-file-path-relative-to-project ()
  (interactive)
  (let ((file-name (file-relative-name buffer-file-name (projectile-project-root))))
    (message file-name)
    (duc/yank-string file-name)))

(defun duc/yank-absolute-path-to-parent ()
  (interactive)
  (let ((parent-directory (file-name-directory (buffer-file-name))))
    (duc/yank-string parent-directory)))

(defvar duc/company-shortcut-keywords
  '(;; Words
    ("ab" "about")
    ("bc" "because")
    ("ck" "check-in")
    ("dnt" "don't")
    ("dont" "don't")
    ("fo" "following")
    ("eg" "e.g.,")
    ("ie" "i.e.,")
    ("isu" "insufficient")
    ("ne" "necessary")
    ("no" "notice")
    ("ol" "outline")
    ("th" "thought")
    ("tk" "think")
    ("rr" "remember")
    ("rp" "responsibilities")
    ("su" "sufficient")
    ("une" "unnecessary")
    ("up" "update")
    ("vs" "versus")
    ("wo" "wonder")
    ("w/" "with") ; TODO doesn't work with slashes.
    ;; Fragments
    ("ai" "action item")
    ("dt" "due to")
    ("fe" "for example")
    ("iow" "in other words")
    ("mdt" "maybe due to")
    ("otoh" "on the other hand")
    ("st" "such that")
    ("wb" "would be")
    ("cop" "class of problems")
    ;; Emotion Matrix
    ("ui" "<unpleasant-intense>")
    ("um" "<unpleasant-mild>")
    ("pi" "<pleasant-intense>")
    ("pm" "<pleasant-mild>")
    ;; Epistemic Status
    ("bs" "<believe-strongly>")
    ("bw" "<believe-weakly>")
    ("bm" "<believe-mildly>")
    ;; Starting sentences
    ("Hs" "How so?")
    ("Wp" "What's the principle behind this?")
    ("Ws" "Why so?")
    ;;; Prompts
    ("Ai" "Action item - ")
    ("Ow" "Outcome wanted - ")
    ("Pe" "Purpose -")
    ;;; Questions
    ("wam" "Why am I feeling this way?")
    ("wwl" "What went well?")
    ("wcb" "What could have gone better?")
    ("wbm" "What might I need to learn, or what strategies might I use the next time to get better results?")))

(defcustom duc/company-shortcut-append-just-one-space nil nil)

(defun duc/company-shortcut--prefix ()
  (let ((wap (thing-at-point 'word 'strip-properties)))
    (when (and wap
               (save-excursion
                 (search-backward wap (line-beginning-position) t)))
      (match-string-no-properties 0))))

(defun duc/company-shortcut--make-candidate (candidate)
  (let ((text (cadr candidate))
        (annotation "S"))
    (propertize text 'annotation annotation)))

(defun duc/company-shortcut--candidates (prefix)
  (let (res)
    (dolist (item duc/company-shortcut-keywords)
      (when (string= prefix (car item))
        (push (duc/company-shortcut--make-candidate item) res)))
    res))

(defun duc/company-shortcut--annotation (candidate)
  (format " (%s)" (get-text-property 0 'annotation candidate)))

;; TODO replace with company-abbrev.
(defun duc/company-shortcut (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'duc/company-shortcut))
    (prefix (duc/company-shortcut--prefix))
    (candidates (duc/company-shortcut--candidates arg))
    (annotation (duc/company-shortcut--annotation arg))
    (kind 'text)
    (post-completion (if duc/company-shortcut-append-just-one-space (just-one-space)))))

(defvar duc/prd-p2c-table-file (expand-file-name "prd-p2c-table.csv" user-emacs-directory))
(defvar duc/prd-saved-dices-file (expand-file-name ".prd-saved-dices.el" user-emacs-directory))
(defvar duc/prd-saved-dices-alist nil
  "Listing of dice with nominal probablities and current miss rolls.
format ((NAME . P MISSES)..)  e.g. ((Omnislash . 0.3 0)
                                    (Multicast . 0.5 2))")

(defun duc/prd-nominal-probability-to-c (p)
  "Function to return an incremental probabilty C
given nomimal probability P.
P is a number between 0.000 and 1.000.

See https://liquipedia.net/dota2/Pseudo_Random_Distribution
See https://observablehq.com/@manuelblanc/pseudo-random-distribution
"
  (with-temp-buffer
    (insert-file-contents duc/prd-p2c-table-file)
    (goto-line (+ (floor (* p 1000)) 1))
    (string-to-number (buffer-substring (line-beginning-position)
                                        (line-end-position)))))

(defun duc/prd-dices-file-save ()
  (when duc/prd-saved-dices-alist
    (let ((out duc/prd-saved-dices-alist))
      (with-temp-file duc/prd-saved-dices-file
        (let ((print-level nil)
              (print-length nil))
          (print out (current-buffer)))))))

(defun duc/prd-dices-file-load ()
  (unless duc/prd-saved-dices-alist
    (with-temp-buffer
      (condition-case nil
          (progn
            (insert-file-contents duc/prd-saved-dices-file)
            (setq duc/prd-saved-dices-alist (read (current-buffer))))
        (error
         (message "Could not read `duc/prd-saved-dices-alist' from %s" duc/prd-saved-dices-file)
         nil)))))

(defun duc/prd-roll (die-to-roll)
  (unless (assoc die-to-roll duc/prd-saved-dices-alist)
    (throw 'unknown-die "Die not in list"))
  (let* ((nums (alist-get die-to-roll duc/prd-saved-dices-alist))
         (p (car nums))
         (c (duc/prd-nominal-probability-to-c p))
         (n (cadr nums))
         (cn (* c (+ n 1)))
         (proc (< (random 1000) (floor (* cn 1000)))))
    ; Update alist die entry
    (setq duc/prd-saved-dices-alist
          (mapcar (lambda (die)
                    (if (eq (car die) die-to-roll)
                        `(,die-to-roll . (,p ,(if proc 0 (+ n 1))))
                      die))
                  duc/prd-saved-dices-alist))
    proc))

(defun duc/prd-roll-and-save (die-to-roll)
  (let ((result (duc/prd-roll die-to-roll)))
    (duc/prd-dices-file-save)
    result))

(defun duc/prd-die-add (die)
  (unless (assoc (car die) duc/prd-saved-dices-alist)
    (push die duc/prd-saved-dices-alist)
    (setq duc/prd-saved-dices-alist
          (sort duc/prd-saved-dices-alist
                (lambda (first second)
                  (string< (car first) (car second)))))
    t))

(provide 'duc)
