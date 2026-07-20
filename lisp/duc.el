;;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(require 'five-letter-words)

;; `ghostel' is loaded lazily, so when this file is byte-compiled its
;; `defcustom' for `ghostel-buffer-name' isn't in scope yet. Declare it special
;; here so the `let'-bindings below (in `duc/ivy-terminal' /
;; `duc/ivy-shell-send-string') bind it dynamically rather than lexically —
;; otherwise Emacs signals "Defining as dynamic an already lexical var".
(defvar ghostel-buffer-name)

(defvar duc/font-family (pcase system-type
                          ('gnu/linux "JetBrains Mono")
                          (_ "InconsolateG for Powerline")))

(defvar duc/font-height (pcase system-type
                          ('windows-nt 100)
                          ('gnu/linux 140)
                          (_ 180)))


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
        ;; ghostel (unlike vterm) has no BUFFER-NAME arg; the new terminal's
        ;; buffer takes its name from `ghostel-buffer-name'.
        (let ((ghostel-buffer-name buffer-name))
          (ghostel))))))

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
    (let* ((buffer-name (if terminal
                            terminal
                          (completing-read "shell : " candidate-terminal-buffers)))
           (existing (member buffer-name candidate-terminal-buffers)))
     (if existing
         (pop-to-buffer buffer-name)
       ;; ghostel names the new buffer from `ghostel-buffer-name'; let-bind it
       ;; so the terminal is created with the requested name.  A fresh shell's
       ;; PTY is spawned in `default-directory', so bind that to
       ;; WORKING-DIRECTORY and skip the `cd' round-trip entirely.
       (let ((ghostel-buffer-name buffer-name)
             (default-directory (if working-directory
                                    (file-name-as-directory
                                     (expand-file-name working-directory))
                                  default-directory)))
         (ghostel)))
     ;; Only an already-running terminal needs an explicit `cd' (its PTY is
     ;; already parked in some other directory).  Paste, don't type: one atomic
     ;; bracketed-paste chunk, then Enter to submit.
     (when (and existing working-directory)
       (ghostel-paste-string (concat "cd " working-directory))
       (ghostel-send-key "return"))
     (when clear
       (ghostel-clear))
     (ghostel-paste-string string)
     (ghostel-send-key "return")
     (pop-to-buffer current-buffer-p))))

;;; Claude Code CLI sessions driven from Org properties
;;
;; An Org entry can describe a running `claude' session via its PROPERTIES
;; drawer:
;;
;;   * Some header
;;   :PROPERTIES:
;;   :CLAUDE_SESSION_ID: 5f3b…            ; the session's identity (dedup key)
;;   :WORKING_DIRECTORY: ~/dev/project     ; optional — start dir for a new one
;;   :TITLE: foobar                        ; arbitrary label shown in the name
;;   :END:
;;
;; A session's identity is its CLAUDE_SESSION_ID; TITLE is an arbitrary label.
;; The tmux session name and ghostel buffer are `ctel TITLE <id8>' /
;; `*ctel TITLE <id8>*', where <id8> is the first 8 chars of CLAUDE_SESSION_ID
;; (see `duc/claude--session-slug').  Invoking `duc/eval-dwim' inside such an
;; entry sends the active region (or the current line) to that `claude' CLI.
;; tmux keeps the conversation alive independently of the Emacs buffer, and lets
;; us inject text by session name with `tmux send-keys' regardless of focus.

(defvar duc/claude-session-ready-delay 2.5
  "Seconds to wait after creating a Claude tmux session before the first send.
Gives the `claude' CLI time to reach its input prompt so the initial message
isn't dropped.")

(defcustom duc/claude-projects-directory "~/.claude/projects"
  "Directory where the Claude Code CLI stores per-project conversation logs.
Each conversation is a `<session-id>.jsonl' under an encoded-cwd subdirectory
\(both `/' and `.' in the path are encoded as `-', so the name is lossy — the
real working directory is read from the log's `cwd' field).
`duc/claude-resume-session' lists these to resume a conversation by id."
  :type 'directory)

(defun duc/claude--nonempty (s)
  "Return S when it is a non-blank string, else nil."
  (and (stringp s) (not (string-blank-p s)) s))

(defun duc/claude--tmux-launch-argv (slug session-id working-directory freshp &optional inner-command)
  "Argv list that creates or attaches to tmux session SLUG running `claude'.
Returns (\"tmux\" \"new-session\" …) suitable for `ghostel-exec' — tmux is the
terminal's own process (no intervening shell), so nothing is echoed as typed
input.  The tmux start-command is the last argv entry, which tmux itself runs
via the default shell.

When FRESHP, start a new conversation pinned to SESSION-ID (`--session-id');
otherwise resume SESSION-ID (`--resume').  WORKING-DIRECTORY, when non-nil, is
the tmux session start directory.

INNER-COMMAND, when non-nil, is used as the tmux start-command instead of the
bare `claude' invocation — an already shell-quoted compound command that itself
ends by exec-ing `claude'.  It is a single argv entry that tmux passes to
`sh -c', so its internal quoting is preserved.  When given, WORKING-DIRECTORY is
ignored (the target dir may not exist yet; INNER-COMMAND cd's into it).

With neither SESSION-ID nor INNER-COMMAND the argv carries no start-command, so
`new-session -A' purely attaches to an existing SLUG (used to reopen a live
session whose id we don't have)."
  (let* ((claude-command
          (cond (inner-command inner-command)
                ((null session-id) nil)
                (freshp (format "claude --session-id %s" (shell-quote-argument session-id)))
                (t (format "claude --resume %s" (shell-quote-argument session-id)))))
         ;; Claude Code enables the Kitty keyboard protocol when the terminal
         ;; advertises it (ghostel's xterm-ghostty TERM does), which encodes a
         ;; lone ESC as an extended `CSI 27 u' sequence rather than a bare \e.
         ;; tmux drops extended keys unless told to forward them, so ESC gets
         ;; eaten in the claude TUI.  Enable extended-keys and advertise the
         ;; extkeys feature for the xterm-ghostty terminal so the CSI-u ESC
         ;; reaches claude.  Also zero `escape-time' (its 500ms default holds a
         ;; bare ESC waiting for a sequence).  `set -s' targets the server this
         ;; session runs on and persists for later `-A' reattaches (which don't
         ;; re-run the start-command).
         (start-command
          (and claude-command
               (concat "tmux set -s escape-time 0 \\; "
                       "set -s extended-keys on \\; "
                       "set -as terminal-features 'xterm*:extkeys' 2>/dev/null; "
                       claude-command))))
    ;; `-A' turns `new-session' into attach-if-exists, so a detached session that
    ;; outlived a killed ghostel buffer is reattached instead of erroring.
    (append (list "tmux" "new-session" "-A" "-s" slug)
            (when (and working-directory (not inner-command))
              (list "-c" working-directory))
            (when start-command (list start-command)))))

(defun duc/claude--tmux-send (slug message)
  "Type MESSAGE (then Enter) into the `claude' prompt in tmux session SLUG.
No-op when the session isn't up yet.  Uses the same default tmux socket as the
session created inside ghostel (both inherit Emacs's environment)."
  (when (and (executable-find "tmux")
             (zerop (call-process "tmux" nil nil nil "has-session" "-t" slug)))
    ;; `-l' sends MESSAGE literally (no key-name interpretation); a separate
    ;; Enter submits it. Multi-line regions are sent as-is — the Claude TUI
    ;; treats an embedded newline as submit, so prefer single lines.
    (call-process "tmux" nil nil nil "send-keys" "-t" slug "-l" message)
    (call-process "tmux" nil nil nil "send-keys" "-t" slug "Enter")))

(defun duc/claude--tmux-safe (s)
  "Return S with characters that break tmux target names replaced by `-'.
tmux splits a target name on `:' (window) and `.' (pane), so a TITLE containing
either — or a newline/tab — would misroute `has-session'/`send-keys'.  Spaces
are fine and are preserved."
  (replace-regexp-in-string "[:.\n\r\t]" "-" (or s "")))

(defun duc/claude--id8 (session-id)
  "Return the short 8-character form of SESSION-ID used in session names."
  (cond ((not (stringp session-id)) "")
        ((>= (length session-id) 8) (substring session-id 0 8))
        (t session-id)))

(defun duc/claude--session-slug (title session-id)
  "Return the tmux session name / ghostel buffer infix for TITLE + SESSION-ID.
Format is `ctel TITLE <id8>', where <id8> is the first 8 characters of
SESSION-ID.  TITLE is an arbitrary label, made tmux-target-safe (see
`duc/claude--tmux-safe'); a blank TITLE collapses the name to `ctel <id8>'.  The
`ctel ' prefix namespaces Claude terminals among other tmux sessions."
  (string-join
   (seq-remove #'string-empty-p
               (list "ctel"
                     (string-trim (duc/claude--tmux-safe title))
                     (duc/claude--id8 session-id)))
   " "))

(defun duc/claude--session-label (title session-id)
  "Return the human list label `TITLE <id8>' for TITLE + SESSION-ID.
Unlike `duc/claude--session-slug' this keeps TITLE verbatim (no `ctel ' prefix,
no tmux sanitising) — it is for display, not for addressing tmux."
  (string-join
   (seq-remove #'string-empty-p
               (list (string-trim (or title ""))
                     (duc/claude--id8 session-id)))
   " "))

(defun duc/claude--terminal-buffer-name (slug)
  "Ghostel buffer name for the Claude session addressed by SLUG.
SLUG is the `ctel TITLE <id8>' tmux session name; the buffer is `*SLUG*'."
  (format "*%s*" slug))

;; A `*ctel …*' buffer's slug carries only the 8-char id, so record the session's
;; full identity on the buffer itself (set by `duc/claude--ensure-terminal').
;; `duc/claude-session-add-to-bnote' and `duc/claude--collect-sessions' read these.
(defvar-local duc/claude--buffer-session-id nil
  "Full CLAUDE_SESSION_ID of the Claude session shown in this ghostel buffer.")

(defvar-local duc/claude--buffer-title nil
  "Arbitrary TITLE label of the Claude session shown in this ghostel buffer.")

(defvar-local duc/claude--buffer-directory nil
  "Working directory of the Claude session shown in this ghostel buffer.")

(defun duc/claude--terminal-live-p (buffer)
  "Non-nil when BUFFER is a ghostel terminal with a live process.
A buffer made by `get-buffer-create' but never initialized by ghostel — e.g.
the `fundamental-mode' husk left behind when `ghostel-exec' signalled, or a
ghostel buffer whose tmux process has since exited — has no live
`ghostel--process' and must not be mistaken for a running terminal."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (bound-and-true-p ghostel--process)
              (process-live-p ghostel--process)))))

(defun duc/claude--session-live-p (slug)
  "Non-nil when a tmux session named SLUG is currently running."
  (and (executable-find "tmux")
       (zerop (call-process "tmux" nil nil nil "has-session" "-t" slug))))

(defun duc/claude--tmux-kill-session (slug)
  "Kill the tmux session named SLUG, ending its `claude' process.
Return non-nil on success.  No-op (returns nil) when tmux is absent or no
session named SLUG is running.  The ghostel buffer and any bnote drawer are
left untouched."
  (and (duc/claude--session-live-p slug)
       (zerop (call-process "tmux" nil nil nil "kill-session" "-t" slug))))

(defun duc/claude--ensure-terminal-slug (slug &optional session-id working-directory freshp inner-command)
  "Create or attach the ghostel + tmux `claude' terminal for tmux session SLUG.
SLUG is both the tmux session name and the ghostel buffer infix (buffer
`*SLUG*').  SESSION-ID / FRESHP / WORKING-DIRECTORY / INNER-COMMAND shape the
tmux start-command (see `duc/claude--tmux-launch-argv'); with none of them a
live SLUG is simply reattached.

Return a plist (:buffer NAME :slug SLUG :created BOOL).  :created is non-nil
only when a new ghostel buffer was spawned this call."
  ;; `ghostel-exec' (and the `ghostel--process' var) are not autoloaded — only
  ;; the interactive `ghostel'/`ghostel-project' entry points are — so load the
  ;; feature before using them, or the first send/resume fails with
  ;; "Symbol's function definition is void: ghostel-exec".
  (require 'ghostel)
  (unless (executable-find "tmux")
    (user-error "tmux not found on `exec-path'; install tmux to use Claude Code sessions"))
  (let* ((buffer-name (duc/claude--terminal-buffer-name slug))
         (existing (get-buffer buffer-name))
         (working-directory (and working-directory (expand-file-name working-directory))))
    (if (duc/claude--terminal-live-p existing)
        (list :buffer buffer-name :slug slug :created nil)
      ;; Any name-matching buffer without a live ghostel process is stale — a
      ;; dead terminal or a `fundamental-mode' husk from a failed `ghostel-exec'.
      ;; Kill it first: `ghostel-exec' refuses a buffer that already has a
      ;; process, and reusing a husk would just redisplay an empty buffer.
      ;; Recreating reattaches a still-detached tmux session via `new-session -A'.
      (when existing
        (let ((kill-buffer-query-functions nil))
          (kill-buffer existing)))
      (save-window-excursion
        ;; Run tmux directly as the terminal's process via `ghostel-exec' (argv
        ;; entries, no intervening shell) rather than typing/pasting a
        ;; `tmux new-session …' line into a shell — so nothing is echoed as
        ;; input and the tmux start-command launches `claude' immediately.
        (let* ((argv (duc/claude--tmux-launch-argv
                      slug session-id working-directory freshp inner-command))
               (buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (ghostel-exec buffer (car argv) (cdr argv)))))
      (list :buffer buffer-name :slug slug :created t))))

(defun duc/claude--ensure-terminal (title &optional session-id working-directory inner-command)
  "Create or attach the ghostel + tmux `claude' terminal for TITLE + SESSION-ID.
The tmux session name and ghostel buffer are derived as `ctel TITLE <id8>' (see
`duc/claude--session-slug'); TITLE is an arbitrary label.  With SESSION-ID,
resume that conversation; otherwise start a fresh one (in WORKING-DIRECTORY when
given) pinned to a newly generated id.

INNER-COMMAND, when non-nil, is a shell-quoted compound command run inside the
tmux session in place of the bare `claude' invocation.

Return a plist (:buffer NAME :slug SLUG :created BOOL :session-id ID).  :created
is non-nil only when a new ghostel buffer was spawned this call; :session-id is
the freshly minted id in that case (else nil, so callers know not to persist)."
  (require 'org-id)
  (let* ((freshp (null session-id))
         (effective-id (or session-id (org-id-uuid)))
         (slug (duc/claude--session-slug title effective-id))
         (info (duc/claude--ensure-terminal-slug
                slug effective-id working-directory freshp inner-command)))
    ;; Stamp the session's full id / label / dir onto the terminal buffer so it
    ;; can identify itself later (the slug carries only the 8-char id).
    (when-let ((buf (get-buffer (plist-get info :buffer))))
      (with-current-buffer buf
        (setq duc/claude--buffer-session-id effective-id
              duc/claude--buffer-title (duc/claude--nonempty title)
              duc/claude--buffer-directory
              (and working-directory (expand-file-name working-directory)))))
    (append info (list :session-id (and freshp effective-id)))))

(defun duc/claude-session-send (message title &optional session-id working-directory)
  "Send MESSAGE to a Claude Code CLI session labelled TITLE, creating it if needed.
The session is addressed by the `ctel TITLE <id8>' tmux/buffer slug (see
`duc/claude--session-slug'); TITLE is an arbitrary label.  With SESSION-ID,
resume that conversation; otherwise start a fresh one (in WORKING-DIRECTORY when
given) pinned to a newly generated id.

Return the generated session id when a fresh session was created (so the caller
can persist it), or nil when an existing terminal was reused."
  (let* ((info (duc/claude--ensure-terminal title session-id working-directory))
         (buffer-name (plist-get info :buffer))
         (slug (plist-get info :slug)))
    (display-buffer buffer-name)
    (if (plist-get info :created)
        ;; Defer the first message until `claude' has had time to boot.
        (run-with-timer duc/claude-session-ready-delay nil
                        #'duc/claude--tmux-send slug message)
      (duc/claude--tmux-send slug message))
    (plist-get info :session-id)))

(defun duc/claude--session-heading-marker ()
  "Marker at the ancestor heading that defines the TITLE property, or nil.
Search runs from point upward so CLAUDE_SESSION_ID is written back onto the
heading that actually owns the Claude-session drawer."
  (save-excursion
    (catch 'found
      (when (org-before-first-heading-p)
        (throw 'found nil))
      (org-back-to-heading t)
      (while t
        (when (org-entry-get (point) "TITLE" nil)
          (throw 'found (point-marker)))
        (unless (org-up-heading-safe)
          (throw 'found nil))))))

(defun duc/claude-session-header-p ()
  "Non-nil when point is inside an Org entry describing a Claude Code session.
Such an entry has a TITLE property together with a CLAUDE_SESSION_ID or a
WORKING_DIRECTORY (looked up with inheritance)."
  (and (derived-mode-p 'org-mode)
       (org-entry-get (point) "TITLE" t)
       (or (org-entry-get (point) "CLAUDE_SESSION_ID" t)
           (org-entry-get (point) "WORKING_DIRECTORY" t))))

(defun duc/claude-session-send-dwim ()
  "Send the active region (or current line) to this entry's Claude session.
Reads TITLE / CLAUDE_SESSION_ID / WORKING_DIRECTORY from the enclosing Org
entry, creates the tmux + ghostel session when needed, and persists a freshly
generated CLAUDE_SESSION_ID back into the drawer when a new session starts."
  (interactive)
  (let* ((title (duc/claude--nonempty (org-entry-get (point) "TITLE" t)))
         (session-id (duc/claude--nonempty (org-entry-get (point) "CLAUDE_SESSION_ID" t)))
         (working-directory (duc/claude--nonempty
                             (org-entry-get (point) "WORKING_DIRECTORY" t)))
         (message (string-trim
                   (if (use-region-p)
                       (buffer-substring-no-properties (region-beginning) (region-end))
                     (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position))))))
    (unless title
      (user-error "No TITLE property for this Claude session entry"))
    (let ((new-id (duc/claude-session-send message title session-id working-directory)))
      ;; Persist the id only when we minted a fresh one, so later sends resume
      ;; the same conversation.
      (when (and new-id (not session-id))
        (let ((marker (duc/claude--session-heading-marker)))
          (org-entry-put (or marker (point)) "CLAUDE_SESSION_ID" new-id)))
      (message "Sent to Claude session %s%s" title
               (if new-id (format " (new %s)" new-id) "")))))

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
                                  (if (eq (line-beginning-position) (line-end-position))
                                      (goto-char (- (point-max) 1)))
                                  (string-match-p (concat repl-prompt "$") (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
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
  (cond ((duc/claude-session-header-p)
         (duc/claude-session-send-dwim))
        ((org-in-src-block-p t)
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
  "Insert an Org link to occurrence X (\"file:line:text\") at the end of buffer."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let ((file-name (match-string-no-properties 1 x)))
      (goto-char (point-max))
      (insert "\n")
      (insert (concat "- [[file:" file-name "][" (completing-read "Link title: " nil) "]]")))))

(defun duc/counsel-ag-insert-linked-link (&optional initial-directory)
  "Grep (ripgrep) under INITIAL-DIRECTORY and insert an Org link to the match.
With a prefix argument, prompt for the directory to search."
  (interactive)
  (let* ((default-directory (or initial-directory
                                (and current-prefix-arg
                                     (read-directory-name "rg in directory: "))
                                (and (fboundp 'vc-root-dir) (vc-root-dir))
                                default-directory))
         (query (read-string "rg: "))
         (candidates
          (and (not (string-empty-p query))
               (split-string
                (shell-command-to-string
                 (format "rg -i --no-heading --line-number --color never %s ."
                         (shell-quote-argument query)))
                "\n" t)))
         (choice (and candidates
                      (completing-read "match: " candidates nil t))))
    (when choice
      (duc/counsel-insert-linked-link-action choice))))

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
                                        ; Copy on disk so the subsequent find-file
                                        ; does the only org-mode init (was: a
                                        ; find-file-noselect + normal-mode pair that
                                        ; initialized org-mode twice).
              (copy-file last-entry new-entry-file)
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

;;; mobile-phoenix worktree launcher + Claude-session log
;;
;; `duc/mp-worktree-create' is a native-elisp port of ~/bin/mp-worktree-create.py.
;; It creates a fresh mobile-phoenix worktree (a new branch off origin/main for
;; implement/qa/other, or a detached checkout of a PR head for review), clones
;; node_modules copy-on-write, and opens a ghostel + tmux `claude' session that
;; runs `yarn install' then drops into an interactive Claude Code prompt in the
;; worktree.  It then records the worktree as a Claude-session drawer under the
;; toplevel `* Claude Sessions' header of today's bnote — carrying TITLE +
;; CLAUDE_SESSION_ID + WORKING_DIRECTORY so `duc/claude-session-send-dwim' /
;; `duc/claude-open-or-create-terminal-session' can drive or resume it later.
;;
;; GUS work-item refs are resolved by shelling out to the shared gus-query.py
;; bridge; PR refs are resolved via `gh'.  The naming (slug / branch / tab title)
;; is kept byte-compatible with the Python script so both tools agree.

(defcustom duc/mp-worktree-repo "~/tb/mobile-phoenix"
  "Main mobile-phoenix checkout that `duc/mp-worktree-create' branches from."
  :type 'string :group 'duc)

(defcustom duc/mp-worktree-tb-dir "~/tb"
  "Directory under which `duc/mp-worktree-create' creates worktree directories."
  :type 'string :group 'duc)

(defcustom duc/mp-worktree-gus-query-script
  "~/.claude/skills/mp-query-fixed-stories-for-qa/scripts/gus-query.py"
  "gus-query.py bridge used by `duc/mp-worktree-create' to resolve GUS refs."
  :type 'string :group 'duc)

(defcustom duc/claude-session-bnote-header "Claude Sessions"
  "Toplevel Org heading (sans stars) under which `duc/mp-worktree-create'
appends a Claude-session drawer in today's bnote.  Created if absent." )

;;; naming helpers (kept byte-compatible with the Python script) ----------

(defun duc/mp-worktree--slugify (text)
  "Lowercase TEXT, collapse non-alphanumerics to single hyphens, trim hyphens."
  (let ((s (replace-regexp-in-string
            "[^a-zA-Z0-9]+" "-" (downcase (string-trim (or text ""))))))
    (string-trim s "-+" "-+")))

(defun duc/mp-worktree--wdigits (wnum)
  "Return the first run of digits in WNUM, or an empty string."
  (if (and wnum (string-match "[0-9]+" wnum)) (match-string 0 wnum) ""))

(defun duc/mp-worktree--clean-subject (subject)
  "Tab-title subject: drop a leading `@?W-1234:'/`W-1234 -' prefix and unwrap a
leading `[Bracket]' to bare text.  Case is preserved."
  (let ((s (string-trim (or subject ""))))
    (setq s (replace-regexp-in-string
             "\\`@?[wW]-[0-9]+[[:space:]]*[:-]?[[:space:]]*" "" s))
    (string-trim
     (replace-regexp-in-string "\\`\\[\\([^]]+\\)\\][[:space:]]*" "\\1 " s))))

(defun duc/mp-worktree--git (repo &rest args)
  "Run `git -C REPO ARGS…' synchronously, returning (EXIT-CODE . TRIMMED-OUTPUT).
Used only for fast, local metadata reads (branch existence, remote URL); the
slow, network-bound steps (fetch, worktree add) go through
`duc/mp-worktree--run-async' so they never block Emacs's UI thread."
  (with-temp-buffer
    (let ((code (apply #'call-process "git" nil t nil "-C" repo args)))
      (cons code (string-trim (buffer-string))))))

(defun duc/mp-worktree--run-async (name command callback)
  "Run COMMAND (a program+args list) asynchronously, then call CALLBACK.
CALLBACK receives (EXIT-CODE TRIMMED-OUTPUT) once the process exits.  NAME
labels the process/buffer.  stdout and stderr are merged.  This is the
non-blocking counterpart to `call-process' — the whole `duc/mp-worktree-create'
flow is a chain of these so the Emacs UI stays responsive (see
https://nullprogram.com/blog/2019/03/10/)."
  (let ((buffer (generate-new-buffer (format " *%s*" name))))
    (make-process
     :name name
     :buffer buffer
     :command command
     :connection-type 'pipe
     :noquery t
     :stderr buffer
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((code (process-exit-status proc))
               (output (with-current-buffer (process-buffer proc)
                         (string-trim (buffer-string)))))
           (kill-buffer (process-buffer proc))
           (funcall callback code output)))))))

;;; GUS reference resolution (via gus-query.py) ----------

(defun duc/mp-worktree--normalize-ref (raw)
  "Turn any accepted GUS ref form in RAW into something gus-query.py classifies.
W-forms collapse to `W-<digits>'; URLs and record ids pass through; nil if empty."
  (let ((r (replace-regexp-in-string "\\`@+" "" (string-trim (or raw "")))))
    (cond
     ((string-empty-p r) nil)
     ((or (string-match-p "ADM_Work__c/" r)
          (string-prefix-p "http" (downcase r)))
      r)
     ((string-match "\\`[wW]-?\\([0-9]+\\)\\'" r) (concat "W-" (match-string 1 r)))
     ((string-match-p "\\`[0-9]+\\'" r) (concat "W-" r))
     (t r))))

(defun duc/mp-worktree--fail (fmt &rest args)
  "Report a `duc/mp-worktree-create' failure and abort the async chain.
Emits a message (chain continuations simply stop calling forward), returning
nil so a sentinel can `(unless …)'-guard on it."
  (message "%s" (apply #'format fmt args))
  nil)

(defun duc/mp-worktree--resolve-gus (raw callback)
  "Resolve GUS ref RAW to (DIGITS NAME SUBJECT), then call CALLBACK with them.
Runs gus-query.py asynchronously so the UI never blocks.  On any failure —
including a browser-requiring reauth (never auto-run) — reports it via
`duc/mp-worktree--fail' and does not call CALLBACK.  Synchronous, instant
validation (ref shape, script presence) still signals a `user-error'."
  (let ((ref (duc/mp-worktree--normalize-ref raw))
        (script (expand-file-name duc/mp-worktree-gus-query-script)))
    (unless ref
      (user-error "mp-worktree-create: could not make sense of GUS ref %S" raw))
    (unless (file-exists-p script)
      (user-error "mp-worktree-create: gus-query.py not found: %s" script))
    (message "mp-worktree-create: resolving GUS %s…" ref)
    (duc/mp-worktree--run-async
     "mp-worktree-gus" (list "python3" script ref)
     (lambda (code output)
       (let ((payload (condition-case nil
                          (json-parse-string output :object-type 'alist
                                              :array-type 'list :null-object nil)
                        (error nil))))
         (cond
          ((or (string-match-p "REAUTH_NEEDED" output)
               (eq t (and payload (alist-get 'reauth_needed payload))))
           (duc/mp-worktree--fail "mp-worktree-create: GUS needs re-authentication (a browser step, not auto-run).  Run `dx auth login' (or the gus-query reauth_cmd) then retry"))
          ((not (and payload (alist-get 'records payload)))
           (duc/mp-worktree--fail "mp-worktree-create: GUS lookup failed for %S%s" ref
                                  (if (zerop code) "" (format " (python3 exit %d)" code))))
          (t
           (let* ((rec (car (alist-get 'records payload)))
                  (name (or (alist-get 'Name rec) ref))
                  (subject (or (alist-get 'Subject__c rec) "")))
             (funcall callback (duc/mp-worktree--wdigits name) name subject)))))))))

;;; PR reference resolution (review command, via gh) ----------

(defun duc/mp-worktree--looks-like-gus-ref (raw)
  "Non-nil when RAW is unmistakably a GUS ref (rejected for the review command)."
  (let ((r (replace-regexp-in-string "\\`@+" "" (string-trim (or raw "")))))
    (or (string-match-p "\\`[wW]-?[0-9]+\\'" r)
        (string-match-p "gus\\.lightning" (downcase r))
        (string-match-p "ADM_Work__c" r)
        (string-match-p "\\`[a-zA-Z0-9]\\{15\\}\\'" r)
        (string-match-p "\\`[a-zA-Z0-9]\\{18\\}\\'" r))))

(defun duc/mp-worktree--normalize-pr-ref (raw)
  "Extract a PR number (as a string) from RAW: a pull URL or 273/PR#273/#273/…."
  (let ((r (string-trim (or raw ""))))
    (cond
     ((string-empty-p r) nil)
     ((string-match "/pull/\\([0-9]+\\)" r) (match-string 1 r))
     ((string-match "\\`\\(?:[pP][rR]\\)?[#-]?\\([0-9]+\\)\\'" r) (match-string 1 r))
     (t nil))))

(defun duc/mp-worktree--repo-slug (repo)
  "owner/name for REPO's origin remote, so `gh -R' works regardless of cwd."
  (let ((url (cdr (duc/mp-worktree--git repo "remote" "get-url" "origin"))))
    (when (string-match "github\\.com[:/]\\([^/]+/[^/]+?\\)\\(?:\\.git\\)?\\'" url)
      (match-string 1 url))))

(defun duc/mp-worktree--resolve-pr (repo raw callback)
  "Resolve PR ref RAW to (NUM TITLE HEAD-OID), then call CALLBACK with them.
Runs `gh pr view' asynchronously so the UI never blocks; reports failures via
`duc/mp-worktree--fail' without calling CALLBACK.  Synchronous, instant
validation (GUS-ref rejection, ref shape) still signals a `user-error'."
  (when (duc/mp-worktree--looks-like-gus-ref raw)
    (user-error "mp-worktree-create: `review' takes a PR reference, not a GUS ref (%s).  Use implement/qa/other for a GUS story, or pass a PR (273 | PR#273 | a pull URL)" raw))
  (let ((num (duc/mp-worktree--normalize-pr-ref raw)))
    (unless num
      (user-error "mp-worktree-create: could not make sense of PR ref %S (want 273 | PR#273 | PR-273 | a github pull URL)" raw))
    (let ((args (append (list "gh" "pr" "view" num)
                        (let ((slug (duc/mp-worktree--repo-slug repo)))
                          (when slug (list "-R" slug)))
                        (list "--json" "number,title,headRefOid,url"))))
      (message "mp-worktree-create: resolving PR #%s…" num)
      (duc/mp-worktree--run-async
       "mp-worktree-gh" args
       (lambda (code output)
         (cond
          ((not (zerop code))
           (duc/mp-worktree--fail "mp-worktree-create: `gh pr view %s' failed:\n%s" num output))
          (t
           (let ((data (condition-case nil
                           (json-parse-string output :object-type 'alist
                                               :null-object nil)
                         (error nil))))
             (if (not data)
                 (duc/mp-worktree--fail "mp-worktree-create: gh returned unparseable output")
               (funcall callback
                        (let ((n (alist-get 'number data)))
                          (if n (number-to-string n) num))
                        (or (alist-get 'title data) "")
                        (or (alist-get 'headRefOid data) "")))))))))))

;;; worktree / branch bookkeeping ----------

(defun duc/mp-worktree--local-branch-exists-p (repo name)
  (zerop (car (duc/mp-worktree--git
               repo "rev-parse" "--verify" "--quiet"
               (concat "refs/heads/" name)))))

(defun duc/mp-worktree--remote-branch-exists-p (repo name)
  (zerop (car (duc/mp-worktree--git
               repo "rev-parse" "--verify" "--quiet"
               (concat "refs/remotes/origin/" name)))))

(defun duc/mp-worktree--unique-names (repo base-dir base-branch)
  "Append -2, -3, … to BASE-DIR/BASE-BRANCH until both path and branch are free.
Returns (PATH . BRANCH)."
  (let ((path base-dir) (branch base-branch) (n 2))
    (while (or (file-exists-p path)
               (duc/mp-worktree--local-branch-exists-p repo branch)
               (duc/mp-worktree--remote-branch-exists-p repo branch))
      (setq path (format "%s-%d" base-dir n)
            branch (format "%s-%d" base-branch n)
            n (1+ n)))
    (cons path branch)))

(defun duc/mp-worktree--primary-node-modules (repo)
  "node_modules of the main checkout — the APFS-clone source for cow mode, or nil."
  (let ((nm (expand-file-name "node_modules" repo)))
    (and (file-directory-p nm) nm)))

(defun duc/mp-worktree--default-prompt (command work subject base source nm-mode)
  "The claude prompt used when --prompt isn't given (the session-handoff skill)."
  (concat "/mp-worktree-create-session-handoff "
          (mapconcat
           #'identity
           (list (format "work=%s" (or work "none"))
                 (format "subject=%s"
                         (let ((c (duc/mp-worktree--clean-subject subject)))
                           (if (string-empty-p c) "(none)" c)))
                 (format "base=%s" base)
                 (format "source=%s" source)
                 (format "command=%s" command)
                 (format "nodeModules=%s (already set up)" nm-mode))
           " | ")))

(defun duc/mp-worktree--parse-args (tokens)
  "Parse TOKENS (shell-split CLI args) into a plist.
Keys :command :ref :nm-mode :prompt :repo :user :dry-run.  `implement' is the
implied default command; review/qa/other must be typed."
  (let ((command "implement") (positionals '())
        (nm-mode "cow") (prompt nil) (repo nil) (user nil) (dry-run nil))
    (cl-labels ((need (val flag) (or val (user-error "mp-worktree-create: %s needs a value" flag)))
                (as-mode (m) (if (member m '("cow" "clean")) m
                               (user-error "mp-worktree-create: --node-modules-dir-mode must be cow or clean"))))
      (while tokens
        (let ((tok (pop tokens)))
          (cond
           ((string= tok "--node-modules-dir-mode")
            (setq nm-mode (as-mode (need (pop tokens) tok))))
           ((string-prefix-p "--node-modules-dir-mode=" tok)
            (setq nm-mode (as-mode (substring tok (length "--node-modules-dir-mode=")))))
           ((string= tok "--prompt") (setq prompt (need (pop tokens) tok)))
           ((string-prefix-p "--prompt=" tok) (setq prompt (substring tok (length "--prompt="))))
           ((string= tok "--repo") (setq repo (need (pop tokens) tok)))
           ((string-prefix-p "--repo=" tok) (setq repo (substring tok (length "--repo="))))
           ((string= tok "--user") (setq user (need (pop tokens) tok)))
           ((string-prefix-p "--user=" tok) (setq user (substring tok (length "--user="))))
           ((string= tok "--dry-run") (setq dry-run t))
           ((string-prefix-p "-" tok) (user-error "mp-worktree-create: unknown option %s" tok))
           (t (push tok positionals))))))
    (setq positionals (nreverse positionals))
    (when (and positionals (member (car positionals) '("implement" "review" "qa" "other")))
      (setq command (pop positionals)))
    (let ((ref (and positionals (pop positionals))))
      (when positionals
        (user-error "mp-worktree-create: unexpected extra arguments: %s"
                    (string-join positionals " ")))
      (list :command command :ref ref :nm-mode nm-mode :prompt prompt
            :repo repo :user user :dry-run dry-run))))

(defun duc/mp-worktree--session-names (tab-title)
  "Derive (HEADING . TITLE) for a Claude-session drawer from the script's TAB-TITLE.
The script formats its tab title as `W<digits> <description>' (or
`PR<num> <description>', or `W? <slug>' for an ad-hoc worktree).  Returns:

  HEADING  the `**' text — `Session [[W:<digits>]] <description>' when there is a
           W-number, else `Session [[PR:<num>]] …' / `Session <description>'.
  TITLE    the CLAUDE_SESSION_ID drawer name — `@W-<digits> <description>' (the
           GUS `@W-' form) when there is a W-number, else the tab title verbatim."
  (cond
   ((string-match "\\`W\\([0-9]+\\)[ \t]*\\(.*\\)\\'" tab-title)
    (let ((digits (match-string 1 tab-title))
          (desc (string-trim (match-string 2 tab-title))))
      (cons (format "Session [[W:%s]]%s" digits
                    (if (string-empty-p desc) "" (concat " " desc)))
            (format "@W-%s%s" digits
                    (if (string-empty-p desc) "" (concat " " desc))))))
   ((string-match "\\`PR\\([0-9]+\\)[ \t]*\\(.*\\)\\'" tab-title)
    (let ((num (match-string 1 tab-title))
          (desc (string-trim (match-string 2 tab-title))))
      (cons (format "Session [[PR:%s]]%s" num
                    (if (string-empty-p desc) "" (concat " " desc)))
            tab-title)))
   (t
    ;; No W#/PR# (e.g. `W? other 20260716…'): keep the tab title for both.
    (cons (format "Session %s" tab-title) tab-title))))

(defun duc/claude--find-session-heading (session-id)
  "Return a marker at the current bnote's `**' session entry for SESSION-ID, or nil.
Searches under the `duc/claude-session-bnote-header' toplevel header for a
level-2 heading whose CLAUDE_SESSION_ID property equals SESSION-ID (a session's
identity).  The current buffer must be today's bnote (an Org buffer)."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^\\* "
                                     (regexp-quote duc/claude-session-bnote-header)
                                     "[ \t]*$")
                             nil t)
      (let ((end (save-excursion (beginning-of-line) (org-end-of-subtree t) (point)))
            found)
        (while (and (not found) (re-search-forward "^\\*\\* " end t))
          (when (equal (org-entry-get (point) "CLAUDE_SESSION_ID") session-id)
            (setq found (point-marker))))
        found))))

(defun duc/claude--append-session-drawer (heading properties)
  "Upsert a Claude-session drawer into today's bnote and return its buffer.
HEADING is the `**' heading text.  PROPERTIES is an alist of (KEY . VALUE)
string pairs; pairs whose VALUE is nil or blank are skipped.

When today's bnote already has a level-2 entry (under the toplevel
`duc/claude-session-bnote-header' header) whose CLAUDE_SESSION_ID property
matches the one in PROPERTIES, that entry is updated in place — its heading
refreshed and its properties set — so re-creating a session (same id) doesn't
duplicate its drawer.
Otherwise a new entry is appended under the header (created if absent), after
any existing content, with one blank line on each side.  Point is left on the
entry heading and the buffer is saved."
  ;; Ensure today's bnote exists and is the current buffer.
  (duc/create-or-open-bnote-type "bnote")
  (let* ((session-id (cdr (assoc "CLAUDE_SESSION_ID" properties)))
         (existing (and (duc/claude--nonempty session-id)
                        (duc/claude--find-session-heading session-id)))
         (live (seq-filter (lambda (kv)
                             (let ((v (cdr kv)))
                               (and (stringp v) (not (string-blank-p v)))))
                           properties)))
    (if existing
        ;; Update in place: refresh the heading text and (re)set each property.
        (progn
          (goto-char existing)
          (org-back-to-heading t)
          (org-edit-headline heading)
          (dolist (kv live)
            (org-entry-put (point) (car kv) (cdr kv)))
          (goto-char existing))
      (goto-char (point-min))
      (if (re-search-forward (concat "^\\* "
                                     (regexp-quote duc/claude-session-bnote-header)
                                     "[ \t]*$")
                             nil t)
          ;; Land at the end of the existing subtree's real content (before the
          ;; blank lines / next top-level header).
          (progn (beginning-of-line) (org-end-of-subtree t))
        ;; No header yet: start one at EOB, separated from any prior content.
        (goto-char (point-max))
        (skip-chars-backward " \t\n")
        (delete-region (point) (point-max))
        (unless (bobp) (insert "\n\n"))
        (insert "* " duc/claude-session-bnote-header))
      (skip-chars-backward " \t\n")
      (let* ((prop-lines
              (mapconcat (lambda (kv) (format ":%s: %s\n" (car kv) (cdr kv)))
                         live ""))
             (entry (concat "** " heading "\n:PROPERTIES:\n" prop-lines ":END:\n")))
        (insert "\n\n" entry)
        ;; Collapse any leftover whitespace and leave exactly one blank line
        ;; before a following heading (or nothing at EOB).
        (let ((entry-start (- (point) (length entry))))
          (when (looking-at "[ \t\n]+")
            (replace-match ""))
          (unless (eobp) (insert "\n"))
          (goto-char entry-start)))))
  (save-buffer)
  (current-buffer))

(defun duc/mp-worktree--log-session (tab-title worktree branch command session-id)
  "Append a Claude-session drawer for a freshly created worktree.
TAB-TITLE is the `W<digits> …' / `PR<num> …' tab title; WORKTREE the worktree
directory; BRANCH its branch (nil = detached); COMMAND the run command; and
SESSION-ID the pinned CLAUDE_SESSION_ID.  Opens today's bnote and inserts a
`**' entry under `duc/claude-session-bnote-header'.  Returns the drawer TITLE."
  (let* ((names (duc/mp-worktree--session-names tab-title))
         (heading (car names))
         (title (cdr names)))
    (duc/claude--append-session-drawer
     heading
     (list (cons "TITLE" title)
           (cons "CLAUDE_SESSION_ID" session-id)
           (cons "WORKING_DIRECTORY" (abbreviate-file-name worktree))
           (cons "BRANCH" (or branch "(detached HEAD)"))
           (cons "COMMAND" command)
           (cons "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))))
    (message "mp-worktree-create: logged Claude session %S" title)
    title))

(defun duc/mp-worktree-create (args-string)
  "Create a mobile-phoenix worktree and open a ghostel + tmux `claude' session.
ARGS-STRING is parsed like the mp-worktree-create.py CLI it ports:

  [command] [ref] [--node-modules-dir-mode cow|clean] [--prompt STR]
                  [--repo DIR] [--user NAME] [--dry-run]

COMMAND is implement (default, may be omitted) | review | qa | other.  For
implement/qa/other a new branch is cut off freshly-fetched origin/main (REF, if
given, is a GUS work-item resolved via gus-query.py for naming + subject).  For
review a detached-HEAD worktree is checked out on an existing PR head (REF is a
required PR reference, resolved via `gh'; a GUS ref here is an error).

node_modules is cloned copy-on-write from the main checkout (cow, the default)
or installed fresh (clean).  The tmux session runs `yarn install' then drops
into `claude', and a Claude-session drawer is appended under the toplevel
`* Claude Sessions' header in today's bnote — ready for `duc/eval-dwim' /
`duc/claude-open-or-create-terminal-session' to drive or resume.

Examples: \"implement W-22371650\", \"W-22371650\", \"review 273\",
\"implement W-22371650 --node-modules-dir-mode clean --prompt fix-reducer\"."
  (interactive (list (read-string "mp-worktree-create " "implement ")))
  (require 'org-id)
  (let* ((opts (duc/mp-worktree--parse-args (split-string-shell-command args-string)))
         (command (plist-get opts :command))
         (ref (plist-get opts :ref))
         (repo (expand-file-name (or (plist-get opts :repo) duc/mp-worktree-repo)))
         (tb (expand-file-name duc/mp-worktree-tb-dir))
         (user (or (plist-get opts :user) (getenv "USER") "dev")))
    (unless (or (file-directory-p (expand-file-name ".git" repo))
                (file-exists-p (expand-file-name ".git" repo)))
      (user-error "mp-worktree-create: %s is not a git checkout (pass --repo)" repo))
    ;; 1) Resolve the ref (async for review/GUS) and decide the worktree shape,
    ;;    then hand a `shape' plist to `duc/mp-worktree--proceed', which drives
    ;;    the fetch → worktree add → node_modules → launch chain.  Everything
    ;;    slow runs through `make-process', so the UI never blocks.
    (cond
     ((string= command "review")
      (unless ref
        (user-error "mp-worktree-create: `review' requires a PR reference (273 | PR#273 | PR-273 | a github pull URL)"))
      (duc/mp-worktree--resolve-pr
       repo ref
       (lambda (num title-text head-oid)
         (let* ((slug (let ((s (duc/mp-worktree--slugify
                                (duc/mp-worktree--clean-subject title-text))))
                        (if (string-empty-p s) (format "pr-%s" num) s)))
                (raw-name (format "mp-pr-%s-%s" num slug)))
           (duc/mp-worktree--proceed
            opts repo
            (list :command command :subject title-text :slug slug
                  ;; Cap the whole basename at 20 chars; trim a dangling hyphen.
                  :base-dir (expand-file-name
                             (string-trim-right
                              (substring raw-name 0 (min 20 (length raw-name))) "-+")
                             tb)
                  :base-branch nil
                  :base-ref (if (string-empty-p head-oid) (format "PR #%s" num) head-oid)
                  :source-desc (format "PR #%s" num)
                  :work-tag (format "PR-%s" num)
                  :tab-head (format "PR%s" num)
                  :pr-num num))))))
     (ref
      (duc/mp-worktree--resolve-gus
       ref
       (lambda (d name subj)
         (let ((slug (let ((s (duc/mp-worktree--slugify subj)))
                       (if (string-empty-p s) (format "w-%s" d) s))))
           (duc/mp-worktree--proceed
            opts repo
            (list :command command :subject subj :slug slug
                  :base-dir (expand-file-name (format "mp-w-%s-%s" d slug) tb)
                  :base-branch (format "dev/%s/w-%s-%s" user d slug)
                  :base-ref "origin/main"
                  :source-desc name
                  :work-tag (format "W-%s" d)
                  :tab-head (format "W%s" d)
                  :pr-num nil))))))
     (t
      ;; No story: name the ad-hoc worktree after the command + a timestamp.
      (let ((slug (format "%s-%s" command (format-time-string "%Y%m%d-%H%M%S"))))
        (duc/mp-worktree--proceed
         opts repo
         (list :command command :subject "" :slug slug
               :base-dir (expand-file-name (format "mp-%s" slug) tb)
               :base-branch (format "dev/%s/%s" user slug)
               :base-ref "origin/main"
               :source-desc "(ad-hoc, no GUS ref)"
               :work-tag "none"
               :tab-head "W?"
               :pr-num nil)))))))

(defun duc/mp-worktree--proceed (opts repo shape)
  "Fetch, add the worktree, clone node_modules, and launch — all async.
OPTS is the parsed option plist; REPO the main checkout; SHAPE the resolved
worktree plist built by `duc/mp-worktree-create' (:command :subject :slug
:base-dir :base-branch :base-ref :source-desc :work-tag :tab-head :pr-num).
Each slow step runs via `duc/mp-worktree--run-async'; a failing step reports
and stops the chain without blocking Emacs."
  (let* ((command (plist-get shape :command))
         (reviewp (string= command "review"))
         (dry-run (plist-get opts :dry-run))
         (nm-mode (plist-get opts :nm-mode))
         (base-dir (plist-get shape :base-dir))
         (base-branch (plist-get shape :base-branch))
         (base-ref (plist-get shape :base-ref))
         (subject (plist-get shape :subject))
         (slug (plist-get shape :slug))
         (pr-num (plist-get shape :pr-num))
         ;; Collision-free names (review only collides on the directory).
         (names (duc/mp-worktree--unique-names
                 repo base-dir (if reviewp (concat "__detached__" slug) base-branch)))
         (path (car names))
         (branch (unless reviewp (cdr names)))
         (clean-subj (duc/mp-worktree--clean-subject subject))
         (title (string-trim
                 (format "%s %s" (plist-get shape :tab-head)
                         (if (string-empty-p clean-subj)
                             (replace-regexp-in-string "-" " " slug)
                           clean-subj))))
         ;; node_modules: cow clones from the main checkout, else clean.
         (src-nm (duc/mp-worktree--primary-node-modules repo))
         (effective-nm (if (and (string= nm-mode "cow") (not src-nm)) "clean" nm-mode))
         (yarn-cmd (if (string= effective-nm "cow")
                       "yarn install --check-files" "yarn install"))
         (prompt (or (plist-get opts :prompt)
                     (duc/mp-worktree--default-prompt
                      command (plist-get shape :work-tag) subject base-ref
                      (plist-get shape :source-desc) effective-nm)))
         (session-id (org-id-uuid)))
    ;; Print the plan (mirrors the script; useful for --dry-run too).
    (message (concat "mp-worktree-create plan:\n"
                     (format "  command    : %s\n" command)
                     (format "  source     : %s\n" (plist-get shape :source-desc))
                     (unless (string-empty-p subject) (format "  subject    : %s\n" subject))
                     (format "  base ref   : %s\n" base-ref)
                     (format "  branch     : %s\n" (or branch "(detached HEAD)"))
                     (format "  worktree   : %s\n" path)
                     (format "  node_modules: %s\n" effective-nm)
                     (format "  tab title  : %s" title)))
    (when (and (string= nm-mode "cow") (not src-nm))
      (message "mp-worktree-create: no node_modules in the main checkout — falling back to clean install"))
    (if dry-run
        (message "mp-worktree-create: --dry-run, created nothing (would open session %S)" title)
      ;; The launch step: open the ghostel + tmux session, then log the drawer.
      (let* ((launch
             (lambda ()
               (let* ((inner (format "cd %s && %s && claude --session-id %s %s"
                                     (shell-quote-argument path) yarn-cmd
                                     (shell-quote-argument session-id)
                                     (shell-quote-argument prompt)))
                      (info (duc/claude--ensure-terminal title session-id nil inner)))
                 (display-buffer (plist-get info :buffer))
                 (save-window-excursion
                   (duc/mp-worktree--log-session title path branch command session-id))
                 (message "mp-worktree-create: created %s → %s" title path))))
            ;; node_modules cow clone (cp -Rc), then launch.  When cow doesn't
            ;; apply, launch straight away — `yarn install' populates it.
            (clone-then-launch
             (lambda ()
               (if (and (string= effective-nm "cow") src-nm)
                   (progn
                     (message "mp-worktree-create: cloning node_modules (APFS cow)…")
                     (duc/mp-worktree--run-async
                      "mp-worktree-cp"
                      (list "cp" "-Rc" src-nm (expand-file-name "node_modules" path))
                      (lambda (code output)
                        (if (zerop code)
                            (message "mp-worktree-create: cloned node_modules (APFS cow) from %s" src-nm)
                          (message "mp-worktree-create: node_modules cow clone failed (%s); `yarn install' will populate it" output))
                        ;; Launch regardless — a failed clone just means a full install.
                        (funcall launch))))
                 (funcall launch)))))
        ;; Chain: fetch → worktree add → clone → launch.
        (message "mp-worktree-create: fetching %s…" (if reviewp (format "PR #%s" pr-num) "origin/main"))
        (duc/mp-worktree--run-async
         "mp-worktree-fetch"
         (append (list "git" "-C" repo "fetch" "origin")
                 (if reviewp (list (format "pull/%s/head" pr-num)) (list "main"))
                 (list "--quiet"))
         (lambda (code output)
           (if (not (zerop code))
               (duc/mp-worktree--fail "mp-worktree-create: fetch of %s failed:\n%s"
                                      (if reviewp (format "PR #%s" pr-num) "origin/main") output)
             (message "mp-worktree-create: adding worktree at %s…" path)
             (duc/mp-worktree--run-async
              "mp-worktree-add"
              (if reviewp
                  (list "git" "-C" repo "worktree" "add" "--detach" path base-ref)
                (list "git" "-C" repo "worktree" "add" "-b" branch path base-ref))
              (lambda (code output)
                (if (not (zerop code))
                    (duc/mp-worktree--fail "mp-worktree-create: `git worktree add' failed:\n%s" output)
                  (funcall clone-then-launch)))))))))))

;;; Discovering and managing Claude Code CLI sessions
;;
;; The commands below present the Claude sessions known to Emacs from three
;; independent sources and let you open, create, and inventory them:
;;
;;   1. running tmux sessions      (`ctel …' names — the source of truth for
;;                                  what's actually alive)
;;   2. live *ctel …* buffers       (the ghostel terminals showing those sessions)
;;   3. bnote Claude-session drawers (the persisted TITLE / CLAUDE_SESSION_ID /
;;                                  WORKING_DIRECTORY / BRANCH metadata)
;;
;; A session's identity is its CLAUDE_SESSION_ID; TITLE is an arbitrary label.
;; The three sources are unioned by the `ctel TITLE <id8>' slug (tmux session
;; name == ghostel buffer infix == derived from a drawer's TITLE + id).

(defun duc/claude--tmux-sessions ()
  "List the names of running Claude tmux sessions (the `ctel …' slugs).
Non-Claude tmux sessions are ignored — Claude terminals are namespaced with the
`ctel ' prefix by `duc/claude--session-slug'."
  (when (executable-find "tmux")
    (with-temp-buffer
      (when (zerop (call-process "tmux" nil t nil
                                 "list-sessions" "-F" "#{session_name}"))
        (seq-filter (lambda (name) (string-prefix-p "ctel " name))
                    (split-string (buffer-string) "\n" t))))))

(defun duc/claude--terminal-buffers ()
  "Return an alist of (SLUG . BUFFER) for live `*ctel …*' ghostel buffers.
SLUG is the `ctel TITLE <id8>' tmux session name (also the buffer infix)."
  (let (result)
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf)))
        (when (and name (string-match "\\`\\*\\(ctel .*\\)\\*\\'" name))
          (push (cons (match-string 1 name) buf) result))))
    (nreverse result)))

(defun duc/claude--bnote-files ()
  "List all bnote Org files, if the bnote directory exists."
  (when (file-directory-p duc/create-bnote-default-dir)
    (directory-files duc/create-bnote-default-dir t "\\`bnote-[0-9]+\\.org\\'")))

(defun duc/claude--scan-bnote-file (file)
  "Return the Claude-session drawers parsed from bnote FILE as a list of plists.
Each plist has :title :session-id :working-directory :branch :command :created
:heading :file :position (the char position of the heading, valid in the saved
file since the parse buffer shares its contents)."
  (let (sessions)
    (with-temp-buffer
      (insert-file-contents file)
      (delay-mode-hooks (org-mode))
      (org-map-entries
       (lambda ()
         (let ((title (org-entry-get nil "TITLE")))
           (when (duc/claude--nonempty title)
             (push (list :title (string-trim title)
                         :session-id (org-entry-get nil "CLAUDE_SESSION_ID")
                         :working-directory (org-entry-get nil "WORKING_DIRECTORY")
                         :branch (org-entry-get nil "BRANCH")
                         :command (org-entry-get nil "COMMAND")
                         :created (org-entry-get nil "CREATED")
                         :heading (org-get-heading t t t t)
                         :file file
                         :position (point))
                   sessions))))
       t))
    (nreverse sessions)))

(defun duc/claude--all-bnote-sessions ()
  "Scan every bnote file for Claude-session drawers.
Return a hash table mapping CLAUDE_SESSION_ID to its newest drawer plist (later
files win).  Drawers lacking a CLAUDE_SESSION_ID are skipped — the id is a
session's identity."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (file (sort (copy-sequence (duc/claude--bnote-files)) #'string<))
      (when (file-readable-p file)
        (dolist (session (duc/claude--scan-bnote-file file))
          (let ((id (duc/claude--nonempty (plist-get session :session-id))))
            (when id (puthash id session table))))))
    table))

(defun duc/claude--session-completions ()
  "Return a sorted list of known Claude session labels (`TITLE <id8>').
Unions bnote drawers, running tmux sessions and live terminal buffers."
  (sort (mapcar (lambda (row) (plist-get row :label))
                (duc/claude--collect-sessions))
        #'string<))

(defun duc/claude--git-branch (dir)
  "Return the current git branch of DIR, or nil when unavailable."
  (let ((dir (and dir (expand-file-name dir))))
    (when (and dir (file-directory-p dir) (executable-find "git"))
      (with-temp-buffer
        (let ((default-directory (file-name-as-directory dir)))
          (when (zerop (call-process "git" nil t nil
                                     "rev-parse" "--abbrev-ref" "HEAD"))
            (duc/claude--nonempty (string-trim (buffer-string)))))))))

(defun duc/claude-open-or-create-terminal-session (input)
  "Open a known Claude session, or start a fresh one titled INPUT.
Completes over the labels (`TITLE <id8>') of sessions known from running tmux
sessions, live terminal buffers, and bnote Claude-session drawers.  When INPUT
matches a known session it is opened — attached if live, else resumed from its
CLAUDE_SESSION_ID.  Otherwise INPUT is treated as a new arbitrary TITLE and a
fresh session is started and logged to today's bnote."
  (interactive (list (completing-read "Claude session (or new title): "
                                      (duc/claude--session-completions))))
  (let* ((rows (duc/claude--collect-sessions))
         (row (seq-find (lambda (r) (equal (plist-get r :label) input)) rows))
         (session-id (and row (duc/claude--nonempty (plist-get row :session-id)))))
    (cond
     ;; Known session with an id: attach-if-live-else-resume, keyed by id.
     (session-id
      (duc/claude-resume-session session-id (plist-get row :directory)))
     ;; Known but idless live session (e.g. resumed from disk, no drawer):
     ;; reattach by its slug.
     ((and row (plist-get row :tmux))
      (pop-to-buffer (plist-get (duc/claude--ensure-terminal-slug
                                 (plist-get row :slug))
                                :buffer)))
     ;; Otherwise treat INPUT as a new title and start a fresh session.
     (t
      (let* ((info (duc/claude--ensure-terminal input))
             (new-id (plist-get info :session-id)))
        (when new-id
          (save-window-excursion
            (duc/claude--append-session-drawer
             (format "Session %s" (duc/claude--session-label input new-id))
             (list (cons "TITLE" input)
                   (cons "CLAUDE_SESSION_ID" new-id)
                   (cons "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))))))
        (pop-to-buffer (plist-get info :buffer)))))))

(defun duc/claude--session-id-p (s)
  "Non-nil when S looks like a Claude session id (a hyphenated UUID)."
  (and (stringp s)
       (string-match-p
        "\\`[0-9a-fA-F]\\{8\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{12\\}\\'"
        (string-trim s))))

(defun duc/claude--jsonl-cwd (file)
  "Return the working directory (`cwd') recorded in Claude log FILE, or nil.
Reads the recorded `cwd' rather than decoding FILE's parent directory name,
which is lossy (Claude encodes both `/' and `.' as `-').  Only a bounded prefix
of FILE is read — `cwd' appears on the first message line, near the top."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file nil 0 65536)
      (goto-char (point-min))
      (when (re-search-forward
             "\"cwd\"[[:space:]]*:[[:space:]]*\"\\(\\(?:[^\"\\]\\|\\\\.\\)*\\)\""
             nil t)
        (let ((raw (match-string 1)))
          (setq raw (replace-regexp-in-string "\\\\/" "/" raw))
          (setq raw (replace-regexp-in-string "\\\\\\\\" "\\\\" raw))
          (duc/claude--nonempty raw))))))

(defun duc/claude--session-id-file (session-id)
  "Return the path to SESSION-ID's Claude conversation log, or nil.
Claude stores each session at
`duc/claude-projects-directory'/<encoded-cwd>/SESSION-ID.jsonl."
  (let* ((root (expand-file-name duc/claude-projects-directory))
         (matches (and (file-directory-p root)
                       (file-expand-wildcards
                        (expand-file-name (concat "*/" session-id ".jsonl") root)))))
    (car matches)))

(defun duc/claude--session-id-directory (session-id)
  "Return the working directory Claude recorded for SESSION-ID, or nil."
  (duc/claude--jsonl-cwd (duc/claude--session-id-file session-id)))

(defun duc/claude--project-cwd (project-dir)
  "Return the `cwd' shared by every session log in encoded PROJECT-DIR.
All logs in a Claude project directory record the same `cwd', so it is read
once (from up to a few logs, in case the first is an empty stub) instead of
per session.  Falls back to a lossy decode of the directory name."
  (let* ((files (file-expand-wildcards (expand-file-name "*.jsonl" project-dir)))
         (cwd (seq-some #'duc/claude--jsonl-cwd (seq-take files 3))))
    (or cwd
        (replace-regexp-in-string
         "-" "/" (file-name-nondirectory (directory-file-name project-dir))))))

(defun duc/claude--disk-sessions ()
  "Return the Claude sessions found on disk as records, newest first.
Each record is a plist (:id :directory :file :mtime).  Scans
`duc/claude-projects-directory'; the working directory is resolved once per
project directory (its sessions all share one cwd)."
  (let* ((root (expand-file-name duc/claude-projects-directory))
         (project-dirs (and (file-directory-p root)
                            (seq-filter #'file-directory-p
                                        (directory-files root t "\\`[^.]"))))
         records)
    (dolist (pd project-dirs)
      (let ((dir (duc/claude--project-cwd pd)))
        (dolist (file (file-expand-wildcards (expand-file-name "*.jsonl" pd)))
          (push (list :id (file-name-base file)
                      :directory dir
                      :file file
                      :mtime (file-attribute-modification-time
                              (file-attributes file)))
                records))))
    (sort records (lambda (a b) (time-less-p (plist-get b :mtime)
                                             (plist-get a :mtime))))))

(defun duc/claude--session-candidate (record)
  "Format RECORD as an aligned completion line `<working-dir>  <date>  <id>'."
  (format "%-38s  %s  %s"
          (let ((d (plist-get record :directory)))
            (if d (abbreviate-file-name (directory-file-name d)) "—"))
          (format-time-string "%Y-%m-%d %H:%M" (plist-get record :mtime))
          (plist-get record :id)))

(defun duc/claude--read-disk-session (prompt)
  "Completing-read a Claude session from disk; return (SESSION-ID . DIRECTORY).
Completes over the sessions under `duc/claude-projects-directory' (each shown as
`<working-dir>  <date>  <id>', newest first); a raw session id may also be typed.
Signals `user-error' for an unrecognised entry."
  (let* ((records (duc/claude--disk-sessions))
         (index (make-hash-table :test 'equal))
         (cands (mapcar (lambda (r)
                          (let ((label (duc/claude--session-candidate r)))
                            (puthash label r index)
                            label))
                        records))
         ;; Keep the newest-first order rather than let completion re-sort.
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata (category . claude-session)
                                 (display-sort-function . identity)
                                 (cycle-sort-function . identity))
                    (complete-with-action action cands string pred))))
         (choice (completing-read prompt table nil nil))
         (record (gethash choice index)))
    (cond
     (record (cons (plist-get record :id) (plist-get record :directory)))
     ((duc/claude--session-id-p choice)
      (let ((id (string-trim choice)))
        (cons id (duc/claude--session-id-directory id))))
     (t (user-error "Not a known Claude session or session id: %s" choice)))))

(defun duc/claude-resume-session (session-id &optional directory)
  "Resume the Claude Code CLI conversation SESSION-ID in DIRECTORY.
Interactively, completes over the Claude sessions found on disk under
`duc/claude-projects-directory', each shown as a `<working-dir>  <date>  <id>'
row — filter by typing part of the working directory (or the date/id).  You may
also type a raw session id that isn't listed.

DIRECTORY defaults to the working directory recorded for SESSION-ID — from a
bnote Claude-session drawer if one tracks it, else from the session's on-disk
log.  The session's TITLE is taken from its drawer, or defaults to DIRECTORY's
base name; the tmux/buffer name is `ctel TITLE <id8>' (see
`duc/claude--session-slug'), so re-resuming the same id reattaches rather than
duplicating.  A live session is reattached; a dead one relaunches via
`claude --resume' after killing any stale terminal buffer (reattaching to a dead
terminal would show only its exited output)."
  (interactive
   (let ((sel (duc/claude--read-disk-session "Resume Claude session (dir/date/id): ")))
     (list (car sel) (cdr sel))))
  (let* ((drawer (gethash session-id (duc/claude--all-bnote-sessions)))
         (directory (or directory
                        (duc/claude--nonempty (plist-get drawer :working-directory))
                        (duc/claude--session-id-directory session-id)))
         (title (or (duc/claude--nonempty (plist-get drawer :title))
                    (and directory
                         (file-name-nondirectory (directory-file-name directory)))
                    ""))
         (slug (duc/claude--session-slug title session-id))
         (live (duc/claude--session-live-p slug)))
    (unless (or live directory)
      (user-error "No working directory found for Claude session %s" session-id))
    ;; A dead session's leftover terminal buffer must go so a fresh
    ;; `claude --resume' relaunches instead of redisplaying exited output.
    (unless live
      (let ((buffer-name (duc/claude--terminal-buffer-name slug)))
        (when (get-buffer buffer-name)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buffer-name)))))
    (let ((info (duc/claude--ensure-terminal title session-id directory)))
      (pop-to-buffer (plist-get info :buffer))
      (message "%s Claude session %s%s"
               (if live "Attached to" "Resuming")
               (duc/claude--session-label title session-id)
               (if (and (not live) directory)
                   (format " in %s" (abbreviate-file-name directory))
                 "")))))

(defun duc/claude--session-context ()
  "Return (SESSION-ID DIRECTORY TITLE) for the Claude session in context, or nil.
Recognises a `*ctel …*' ghostel terminal buffer (via the buffer-locals stamped
by `duc/claude--ensure-terminal') and a line of `duc/claude-sessions-mode' (via
its row).  Returns nil when neither applies or the row carries no id."
  (cond
   ((bound-and-true-p duc/claude--buffer-session-id)
    (list duc/claude--buffer-session-id
          duc/claude--buffer-directory
          duc/claude--buffer-title))
   ((derived-mode-p 'duc/claude-sessions-mode)
    (let* ((row (duc/claude--sessions-row-at-point))
           (id (and row (plist-get row :session-id))))
      (when id
        (list id
              (plist-get row :directory)
              (plist-get (plist-get row :drawer) :title)))))))

(defun duc/claude-session-add-to-bnote (session-id &optional directory title)
  "Register the Claude session SESSION-ID in today's bnote, unless already tracked.
Writes a Claude-session drawer (TITLE / CLAUDE_SESSION_ID / WORKING_DIRECTORY /
BRANCH / CREATED) under the `duc/claude-session-bnote-header' header, so a session
that so far exists only on disk becomes tracked — listed by
`duc/claude-list-all-terminal-sessions' and resumable by id from there.

Invoked from a `*ctel …*' terminal buffer it adds that buffer's session; on a
line of `duc/claude-sessions-mode' it adds that row's session; elsewhere it
completes over the on-disk sessions (or a raw id) and prompts for a TITLE.  When
the session already has a drawer in any bnote it is not duplicated — point jumps
to the existing entry instead.  TITLE is an arbitrary label (default: DIRECTORY's
base name)."
  (interactive
   (or (duc/claude--session-context)
       (let* ((sel (duc/claude--read-disk-session
                    "Add Claude session to bnote (dir/date/id): "))
              (dir (cdr sel)))
         (list (car sel) dir
               (read-string "Session title: "
                            (and dir (file-name-nondirectory
                                      (directory-file-name dir))))))))
  (let ((existing (gethash session-id (duc/claude--all-bnote-sessions))))
    (if existing
        ;; Already tracked: don't duplicate — reveal the existing drawer.
        (progn
          (find-file (plist-get existing :file))
          (goto-char (plist-get existing :position))
          (when (fboundp 'org-fold-show-entry) (org-fold-show-entry))
          (message "Claude session %s is already in %s"
                   (duc/claude--session-label
                    (or (duc/claude--nonempty title) (plist-get existing :title))
                    session-id)
                   (abbreviate-file-name (plist-get existing :file))))
      (let* ((directory (or directory (duc/claude--session-id-directory session-id)))
             (title (or (duc/claude--nonempty title)
                        (and directory (file-name-nondirectory
                                        (directory-file-name directory)))
                        ""))
             (file (duc/claude--session-id-file session-id))
             ;; Date the drawer from the session log's mtime (when it was last
             ;; active), falling back to now if the log isn't on disk.
             (created (format-time-string
                       "[%Y-%m-%d %a %H:%M]"
                       (and file (file-attribute-modification-time
                                  (file-attributes file)))))
             (buffer (duc/claude--append-session-drawer
                      (format "Session %s" (duc/claude--session-label title session-id))
                      (list (cons "TITLE" title)
                            (cons "CLAUDE_SESSION_ID" session-id)
                            (cons "WORKING_DIRECTORY"
                                  (and directory (abbreviate-file-name directory)))
                            (cons "BRANCH" (duc/claude--git-branch directory))
                            (cons "CREATED" created)))))
        (pop-to-buffer buffer)
        (when (fboundp 'org-fold-show-entry) (org-fold-show-entry))
        (message "Added Claude session %s to %s"
                 (duc/claude--session-label title session-id)
                 (abbreviate-file-name (or (buffer-file-name buffer) "bnote")))))))

(defun duc/claude-new-session-at-working-directory (directory &optional title)
  "Start a fresh Claude Code CLI session in DIRECTORY and log it to today's bnote.
Prompts for the working directory and a session TITLE (default: the directory's
base name).  Mints a CLAUDE_SESSION_ID, records a Claude-session drawer (with
WORKING_DIRECTORY, BRANCH and CREATED) under the toplevel Claude-sessions
header, and shows the terminal."
  (interactive
   (let* ((dir (read-directory-name
                "Claude session working directory: "
                (or (and (fboundp 'projectile-project-root)
                         (ignore-errors (projectile-project-root)))
                    default-directory)))
          (default-title (file-name-nondirectory (directory-file-name dir))))
     (list dir (read-string "Session title: " default-title))))
  (let* ((directory (expand-file-name directory))
         (title (or (duc/claude--nonempty title)
                    (file-name-nondirectory (directory-file-name directory))))
         (info (duc/claude--ensure-terminal title nil directory))
         (new-id (plist-get info :session-id))
         (branch (duc/claude--git-branch directory)))
    (save-window-excursion
      (duc/claude--append-session-drawer
       (format "Session %s" (duc/claude--session-label title new-id))
       (list (cons "TITLE" title)
             (cons "CLAUDE_SESSION_ID" new-id)
             (cons "WORKING_DIRECTORY" (abbreviate-file-name directory))
             (cons "BRANCH" branch)
             (cons "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]")))))
    (pop-to-buffer (plist-get info :buffer))
    (message "Started Claude session %s at %s" title
             (abbreviate-file-name directory))))

;; --- tabulated inventory of all sessions ------------------------------------

(defvar-local duc/claude--session-rows nil
  "Row plists backing the current *Claude sessions* list buffer.")

(defun duc/claude--collect-sessions ()
  "Gather every known Claude session as a list of row plists, sorted by label.
Unions bnote drawers, running tmux sessions and live terminal buffers by their
`ctel TITLE <id8>' slug (the tmux session name / buffer infix), then annotates
each with its label, session id, liveness, working directory and git branch."
  (let* ((drawers (duc/claude--all-bnote-sessions))
         (tmux (duc/claude--tmux-sessions))
         (buffers (duc/claude--terminal-buffers))
         ;; slug -> drawer, computed from each drawer's TITLE + id.
         (drawer-by-slug (make-hash-table :test 'equal))
         (slugs (make-hash-table :test 'equal))
         rows)
    (maphash (lambda (_id drawer)
               (let ((slug (duc/claude--session-slug
                            (plist-get drawer :title)
                            (plist-get drawer :session-id))))
                 (puthash slug drawer drawer-by-slug)
                 (puthash slug t slugs)))
             drawers)
    (dolist (name tmux) (puthash name t slugs))
    (dolist (cell buffers) (puthash (car cell) t slugs))
    (maphash
     (lambda (slug _)
       (let* ((drawer (gethash slug drawer-by-slug))
              (buffer (cdr (assoc slug buffers)))
              ;; A live buffer knows its own full id / dir even without a drawer.
              (session-id (or (duc/claude--nonempty (plist-get drawer :session-id))
                              (and buffer (buffer-local-value
                                           'duc/claude--buffer-session-id buffer))))
              (label (if drawer
                         (duc/claude--session-label (plist-get drawer :title) session-id)
                       ;; Orphan slug (no drawer): its label is the slug minus
                       ;; the `ctel ' prefix.
                       (string-remove-prefix "ctel " slug)))
              (dir (or (duc/claude--nonempty (plist-get drawer :working-directory))
                       (and buffer (duc/claude--nonempty
                                    (buffer-local-value 'duc/claude--buffer-directory buffer)))
                       (and buffer (buffer-local-value 'default-directory buffer))))
              (branch (or (duc/claude--nonempty (plist-get drawer :branch))
                          (duc/claude--git-branch dir))))
         (push (list :slug slug
                     :label label
                     :session-id session-id
                     :buffer buffer
                     :tmux (and (member slug tmux) t)
                     :directory dir
                     :branch branch
                     :drawer drawer)
               rows)))
     slugs)
    (sort rows (lambda (a b) (string< (plist-get a :label)
                                      (plist-get b :label))))))

(defun duc/claude--sessions-refresh ()
  "Recompute `tabulated-list-entries' for the *Claude sessions* buffer.
Each entry is keyed by the session slug so the row commands can recover it."
  (let ((rows (duc/claude--collect-sessions)))
    (setq duc/claude--session-rows rows)
    (setq tabulated-list-entries
          (mapcar
           (lambda (row)
             (list (plist-get row :slug)
                   (vector
                    (or (plist-get row :label) "—")
                    (if (plist-get row :buffer) "yes" "—")
                    (if (plist-get row :tmux) "run" "—")
                    (let ((d (plist-get row :directory)))
                      (if d (abbreviate-file-name (directory-file-name d)) "—"))
                    (or (plist-get row :branch) "—"))))
           rows))))

(defvar duc/claude-sessions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'duc/claude-sessions-open)
    (define-key map (kbd "o") #'duc/claude-sessions-open)
    (define-key map (kbd "r") #'duc/claude-sessions-resume)
    (define-key map (kbd "a") #'duc/claude-session-add-to-bnote)
    (define-key map (kbd "j") #'duc/claude-sessions-visit-drawer)
    (define-key map (kbd "k") #'duc/claude-session-kill-tmux-session)
    map)
  "Keymap for `duc/claude-sessions-mode'.")

(define-derived-mode duc/claude-sessions-mode tabulated-list-mode "Claude-Sessions"
  "Major mode listing all known Claude Code CLI sessions."
  (setq tabulated-list-format
        [("Session" 40 t)
         ("Buf" 4 t)
         ("tmux" 5 t)
         ("Dir" 44 t)
         ("Branch" 26 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key '("Session" . nil))
  (add-hook 'tabulated-list-revert-hook #'duc/claude--sessions-refresh nil t)
  (tabulated-list-init-header))

(defun duc/claude--sessions-row-at-point ()
  "Return the row plist for the session on the current list line, or nil."
  (let ((slug (tabulated-list-get-id)))
    (and slug (seq-find (lambda (r) (equal (plist-get r :slug) slug))
                        duc/claude--session-rows))))

(defun duc/claude-sessions-open ()
  "Open the Claude session on the current list line (attach if live, else resume)."
  (interactive)
  (let* ((row (duc/claude--sessions-row-at-point))
         (id (and row (plist-get row :session-id))))
    (cond
     ((null row) (user-error "No session on this line"))
     (id (duc/claude-resume-session id (plist-get row :directory)))
     ((plist-get row :tmux)
      (pop-to-buffer (plist-get (duc/claude--ensure-terminal-slug
                                 (plist-get row :slug))
                                :buffer)))
     (t (user-error "Session %s has no id to resume and no running tmux session"
                    (plist-get row :label))))))

(defun duc/claude-sessions-resume ()
  "Resume the Claude session on the current list line in its working directory."
  (interactive)
  (let* ((row (duc/claude--sessions-row-at-point))
         (id (and row (plist-get row :session-id))))
    (cond
     ((null row) (user-error "No session on this line"))
     (id (duc/claude-resume-session id (plist-get row :directory)))
     (t (user-error "No CLAUDE_SESSION_ID recorded for %s" (plist-get row :label))))))

(defun duc/claude-sessions-visit-drawer ()
  "Visit the bnote Claude-session drawer for the session on the current line."
  (interactive)
  (let* ((row (duc/claude--sessions-row-at-point))
         (drawer (plist-get row :drawer)))
    (if drawer
        (progn
          (find-file (plist-get drawer :file))
          (goto-char (plist-get drawer :position))
          (when (fboundp 'org-fold-show-entry) (org-fold-show-entry)))
      (user-error "No bnote drawer for session %s"
                  (if row (plist-get row :label) "on this line")))))

(defun duc/claude-session-kill-tmux-session ()
  "Kill the tmux session for the Claude session on the current list line.
Only for `duc/claude-sessions-mode'.  Prompts for confirmation, then ends the
`claude' process by killing its tmux session, leaving the ghostel buffer and any
bnote drawer intact, and refreshes the list so the `tmux' column updates."
  (interactive)
  (unless (derived-mode-p 'duc/claude-sessions-mode)
    (user-error "Not in a Claude sessions list"))
  (let* ((row (duc/claude--sessions-row-at-point))
         (slug (and row (plist-get row :slug)))
         (label (and row (plist-get row :label))))
    (unless row
      (user-error "No session on this line"))
    (unless (duc/claude--session-live-p slug)
      (user-error "No running tmux session for %s" label))
    (when (yes-or-no-p (format "Kill tmux session %s (ends its claude process)? "
                               label))
      (if (duc/claude--tmux-kill-session slug)
          (progn
            (revert-buffer)
            (message "Killed tmux session %s" label))
        (message "Failed to kill tmux session %s" label)))))

(defun duc/claude-list-all-terminal-sessions ()
  "List all Claude Code CLI sessions in a tabulated buffer.
Columns: session label (`TITLE <id8>'), whether an Emacs terminal buffer exists,
whether a tmux session is running, working directory, and git branch.  Data is
unioned from bnote Claude-session drawers, running tmux sessions, and live
`*ctel …*' buffers.  RET/o opens the session under point (attach if live, else
resume); `r' resumes it in its working directory; `a' adds it to today's bnote;
`j' visits its bnote drawer; `k' kills its tmux session; `g' refreshes."
  (interactive)
  (let ((buffer (get-buffer-create "*Claude sessions*")))
    (with-current-buffer buffer
      (duc/claude-sessions-mode)
      (duc/claude--sessions-refresh)
      (tabulated-list-print))
    (pop-to-buffer buffer)))

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
  "Recursively find a file under `default-directory' (vertico + consult-find)."
  (interactive)
  (consult-find default-directory))

(defun duc/incremental-search-filenames-in-version-control ()
  "Find a file tracked in the current project (vertico + project.el)."
  (interactive)
  (project-find-file))

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

(defcustom duc/company-shortcut-append-just-one-space nil
  "Whether to append just one space after a company shortcut expansion."
  :type 'boolean
  :group 'duc)

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
    (goto-char (point-min))
    (forward-line (floor (* p 1000)))
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

;; below the line of confidence are functions under review

(defun org-worklog-entry (message)
  "Insert a timestamped entry under the '* worklog' heading.
Prompts for MESSAGE to log with current timestamp."
  (interactive "sWorklog message: ")
  (duc/create-or-open-bnote-type "wlog")
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\* worklog$" nil t)
        ;; Found worklog heading
        (progn
          (end-of-line)
          ;; Search for the next heading or end of buffer
          (let ((next-heading (save-excursion
                               (if (re-search-forward "^\\*+ " nil t)
                                   (line-beginning-position)
                                 (point-max)))))
            ;; Go just before the next heading or end of buffer
            (goto-char next-heading)
            ;; Skip backward over blank lines
            (skip-chars-backward " \t\n")
            ;; Insert new entry on a new line
            (insert "\n" (format "- <%s> %s"
                               (format-time-string "%Y-%m-%d %a %H:%M")
                               message))))
      ;; No worklog heading found, create it at the end
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* worklog\n")
      (insert (format "- <%s> %s"
                     (format-time-string "%Y-%m-%d %a %H:%M")
                     message))))
  (message "Worklog entry added: %s" message))

(provide 'duc)
