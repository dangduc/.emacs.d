;;; agent-sidebar.el --- Sidebar browser for AI agent chats -*- lexical-binding: t; coding: utf-8; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Keywords: agent-shell, claude, codex, tools
;; Package-Requires: ((emacs "29.1") (agent-shell "0.60") (project "0.9"))

;;; Commentary:
;;
;; Persistent sidebar that lists chats/conversations from AI agent tools
;; installed on this machine.  Each source is a "provider" (agent-shell,
;; Claude CLI, etc.); entries can be nested and grouped by any combination
;; of `package', `repo', and `model' via `agent-sidebar-grouping'.
;;
;; RET on a chat row dispatches through the entry's provider:
;;   - agent-shell entries: resume via `agent-shell--start' when a session
;;     id is present, else start a fresh shell in the recorded cwd.
;;   - Claude CLI entries: spawn `claude --resume <uuid>' in a terminal
;;     (ghostel/vterm/term) at the recorded cwd.
;;
;; Metadata parsing (agent name, first user prompt, model) runs lazily on
;; an idle-timer in chunks so hundreds of transcripts don't block on
;; refresh.  Parse results are cached keyed by (path . mtime) plus a
;; schema version so parser changes invalidate stale entries.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'project)
(require 'agent-shell)

(declare-function projectile-known-projects "projectile")
(defvar projectile-known-projects)

(declare-function evil-define-key* "evil-core")
(declare-function evil-make-overriding-map "evil-core")
(declare-function evil-goto-first-line "evil-commands")
(declare-function evil-goto-line "evil-commands")

(declare-function ghostel-exec "ghostel")
(declare-function vterm "vterm")
(defvar vterm-shell)

;;; Customization

(defgroup agent-sidebar nil
  "Sidebar browser for AI agent chat transcripts across providers."
  :group 'tools)

(defcustom agent-sidebar-name "*:AgentChats:*"
  "Name of the sidebar buffer."
  :type 'string)

(defcustom agent-sidebar-width 55
  "Width of the sidebar window."
  :type 'integer)

(defcustom agent-sidebar-display-alist '((side . left) (slot . 2))
  "Alist passed to `display-buffer-in-side-window'."
  :type 'alist)

(defcustom agent-sidebar-pop-to-sidebar-on-toggle-open t
  "Whether to select the sidebar window after toggling it open."
  :type 'boolean)

(defcustom agent-sidebar-no-delete-other-windows t
  "Whether the sidebar window survives `delete-other-windows'."
  :type 'boolean)

(defcustom agent-sidebar-resize-on-open t
  "Whether to resize the sidebar window when opening it."
  :type 'boolean)

(defcustom agent-sidebar-window-fixed 'width
  "Value for `window-size-fixed' in sidebar buffers."
  :type '(choice (const :tag "Not fixed" nil)
                 (const :tag "Fixed width" width)
                 (const :tag "Fixed height" height)))

(defcustom agent-sidebar-parse-chunk-size 30
  "Number of entry headers to parse per idle tick."
  :type 'integer)

(defcustom agent-sidebar-parse-idle-delay 0.1
  "Idle delay in seconds between parse chunks."
  :type 'number)

(defcustom agent-sidebar-open-file-in-most-recently-used-window t
  "Whether visited chats open in the MRU window."
  :type 'boolean)

(defcustom agent-sidebar-refresh-timer 30
  "Auto-refresh the sidebar every N seconds when idle.  Nil disables."
  :type '(choice (const :tag "Disabled" nil) integer))

(defcustom agent-sidebar-extra-project-roots nil
  "Additional project roots to scan.
Merged with `project-known-project-roots' and
`projectile-known-projects'."
  :type '(repeat directory))

(defcustom agent-sidebar-collapse-empty-projects t
  "Hide projects with no entries from the sidebar."
  :type 'boolean)

(defcustom agent-sidebar-grouping '(package repo model)
  "Grouping levels for the sidebar tree, outermost first.
Each element is one of the recognized levels:
  `package' — group by provider (e.g. agent-shell, Claude CLI)
  `repo'    — group by project/repository root
  `model'   — group by agent/model name (Claude, Codex, ...)
  `date'    — group by YYYY-MM-DD (from parsed timestamp or file mtime)

An empty list produces a flat listing sorted by date."
  :type '(repeat (choice (const package)
                         (const repo)
                         (const model)
                         (const date))))

(defcustom agent-sidebar-enabled-providers '(agent-shell claude-cli)
  "Provider ids to include in the sidebar.
See `agent-sidebar-providers' for available providers."
  :type '(repeat symbol))

(defcustom agent-sidebar-claude-cli-command "claude"
  "Executable name for the Claude CLI."
  :type 'string)

(defcustom agent-sidebar-claude-cli-resume-args
  (list "--resume")
  "Args prepended before the session UUID when resuming a Claude CLI chat.
The UUID is appended as the final argument."
  :type '(repeat string))

(defcustom agent-sidebar-claude-cli-projects-dir
  (expand-file-name "~/.claude/projects/")
  "Directory where Claude CLI stores per-project session JSONLs."
  :type 'directory)

(defcustom agent-sidebar-terminal-function nil
  "Function to run PROGRAM with ARGS in a terminal at DIRECTORY.
Called as (NAME PROGRAM ARGS DIRECTORY).  When nil, `agent-sidebar'
tries ghostel, then vterm, then `term'."
  :type '(choice (const :tag "Auto (ghostel > vterm > term)" nil)
                 function))

;;; Faces

(defface agent-sidebar-package-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for provider/package header rows.")

(defface agent-sidebar-project-face
  '((t :inherit dired-directory :weight bold))
  "Face for project header rows.")

(defface agent-sidebar-agent-face
  '((t :inherit font-lock-keyword-face))
  "Face for agent/model header rows.")

(defface agent-sidebar-date-face
  '((t :inherit font-lock-constant-face))
  "Face for entry date column.")

(defface agent-sidebar-mark-face
  '((t :inherit warning))
  "Face for row marks (e.g. delete).")

(defface agent-sidebar-placeholder-face
  '((t :inherit shadow))
  "Face for un-parsed placeholder metadata.")

;;; State

(defconst agent-sidebar--cache-schema 3
  "Bump when the parsed metadata format changes to invalidate old entries.")

(defvar agent-sidebar--parse-cache (make-hash-table :test 'equal)
  "Maps entry id -> (:schema INT :mtime FLOAT :meta PLIST).
The meta plist keys are: :agent :model :timestamp :cwd :session-id :preview.")

(defvar agent-sidebar--parse-timer nil
  "Active idle timer for chunked metadata parsing.")

(defvar agent-sidebar--parse-queue nil
  "List of entries awaiting metadata parse.")

(defvar-local agent-sidebar--marks nil
  "Hash table mapping entry id -> mark symbol (e.g. `delete').")

(defvar-local agent-sidebar--collapsed nil
  "Hash table of collapsed group breadcrumbs (list of level keys).")

(defvar-local agent-sidebar--refresh-timer-object nil
  "Per-buffer idle timer for auto-refresh.")

;;; Project roots

(defun agent-sidebar--project-roots ()
  "Return the union of known project roots, deduplicated and normalized."
  (let ((roots (append
                (when (fboundp 'project-known-project-roots)
                  (project-known-project-roots))
                (when (bound-and-true-p projectile-known-projects)
                  projectile-known-projects)
                agent-sidebar-extra-project-roots)))
    (thread-last roots
                 (mapcar (lambda (r)
                           (when r
                             (expand-file-name (file-name-as-directory r)))))
                 (delq nil)
                 (seq-uniq))))

(defun agent-sidebar--disambiguated-names (roots)
  "Return a hash table mapping each ROOT to a unique display name.
Uses the shortest trailing path suffix that is unique across ROOTS."
  (let* ((splits (mapcar (lambda (r)
                           (cons r
                                 (nreverse
                                  (split-string
                                   (directory-file-name r) "/" t))))
                         roots))
         (depths (make-hash-table :test 'equal))
         (result (make-hash-table :test 'equal)))
    (dolist (pair splits)
      (puthash (car pair) 1 depths))
    (let ((changed t))
      (while changed
        (setq changed nil)
        (let ((by-name (make-hash-table :test 'equal)))
          (dolist (pair splits)
            (let* ((r (car pair))
                   (parts (cdr pair))
                   (d (min (gethash r depths) (length parts)))
                   (name (string-join
                          (nreverse (seq-take parts d)) "/")))
              (push r (gethash name by-name))))
          (maphash
           (lambda (_name rs)
             (when (> (length rs) 1)
               (dolist (r rs)
                 (let* ((pair (assoc r splits))
                        (parts (cdr pair))
                        (d (gethash r depths)))
                   (when (< d (length parts))
                     (puthash r (1+ d) depths)
                     (setq changed t))))))
           by-name))))
    (dolist (pair splits)
      (let* ((r (car pair))
             (parts (cdr pair))
             (d (min (gethash r depths) (length parts))))
        (puthash r
                 (string-join (nreverse (seq-take parts d)) "/")
                 result)))
    result))

;;; Terminal launcher

(defun agent-sidebar--launch-terminal (name program args directory)
  "Launch PROGRAM ARGS in a terminal buffer named NAME at DIRECTORY."
  (if agent-sidebar-terminal-function
      (funcall agent-sidebar-terminal-function name program args directory)
    (cond
     ((fboundp 'ghostel-exec)
      (let* ((default-directory (file-name-as-directory directory))
             (buffer (generate-new-buffer name)))
        (with-current-buffer buffer
          (ghostel-exec buffer program args))
        (agent-sidebar--pop-to buffer)
        buffer))
     ((fboundp 'vterm)
      (let* ((default-directory (file-name-as-directory directory))
             (vterm-shell (mapconcat #'shell-quote-argument
                                     (cons program args) " ")))
        (funcall (symbol-function 'vterm) name)))
     (t
      (let ((default-directory (file-name-as-directory directory)))
        (term (mapconcat #'shell-quote-argument
                         (cons program args) " ")))))))

;;; Provider protocol
;;
;; A provider is a plist with the keys:
;;   :id       symbol, e.g. `agent-shell'
;;   :name     display string, e.g. "agent-shell"
;;   :list     () -> list of ENTRY plists (metadata may be partial)
;;   :parse    (ENTRY) -> ENTRY with :meta filled in
;;   :visit    (ENTRY) -> side-effect (opens/resumes)
;;
;; An ENTRY is a plist with:
;;   :provider SYMBOL             (provider id)
;;   :id       STRING             (globally unique; usually the source file path)
;;   :repo     STRING or nil      (project root path)
;;   :agent    STRING or nil      (fast-known agent/model label; else parsed)
;;   :mtime    FLOAT              (for sorting and cache validation)
;;   :extras   PLIST              (provider-specific data, e.g. session-id)

(defvar agent-sidebar-providers nil
  "Alist of (ID . PROVIDER-PLIST).")

(defun agent-sidebar-register-provider (provider)
  "Register or replace PROVIDER (a plist)."
  (let ((id (plist-get provider :id)))
    (setq agent-sidebar-providers
          (cons (cons id provider)
                (assq-delete-all id agent-sidebar-providers)))))

(defun agent-sidebar--enabled-providers ()
  "Return the enabled providers as plists in configured order."
  (delq nil
        (mapcar (lambda (id) (alist-get id agent-sidebar-providers))
                agent-sidebar-enabled-providers)))

(defun agent-sidebar--entry-provider (entry)
  "Return the provider plist for ENTRY."
  (alist-get (plist-get entry :provider) agent-sidebar-providers))

;;; Cache

(defun agent-sidebar--cached-meta (entry)
  "Return cached metadata plist for ENTRY if fresh, else nil."
  (when-let* ((id (plist-get entry :id))
              (mtime (plist-get entry :mtime))
              (cell (gethash id agent-sidebar--parse-cache))
              (schema (plist-get cell :schema))
              ((equal schema agent-sidebar--cache-schema))
              (cached-mtime (plist-get cell :mtime))
              ((equal cached-mtime mtime)))
    (plist-get cell :meta)))

(defun agent-sidebar--put-cache (entry meta)
  "Store META for ENTRY in the parse cache."
  (puthash (plist-get entry :id)
           (list :schema agent-sidebar--cache-schema
                 :mtime (plist-get entry :mtime)
                 :meta meta)
           agent-sidebar--parse-cache))

(defun agent-sidebar--ensure-parsed (entry)
  "Return metadata for ENTRY, parsing synchronously on cache miss."
  (or (agent-sidebar--cached-meta entry)
      (let* ((provider (agent-sidebar--entry-provider entry))
             (parser (plist-get provider :parse))
             (meta (and parser (funcall parser entry))))
        (agent-sidebar--put-cache entry meta)
        meta)))

;;; Async parse loop

(defun agent-sidebar--queue-uncached (entries)
  "Push ENTRIES lacking a fresh cache entry onto the parse queue."
  (dolist (e entries)
    (unless (agent-sidebar--cached-meta e)
      (unless (member e agent-sidebar--parse-queue)
        (setq agent-sidebar--parse-queue
              (nconc agent-sidebar--parse-queue (list e)))))))

(defun agent-sidebar--start-parse-timer ()
  "Kick off the idle timer if there is work to do and it's not running."
  (when (and agent-sidebar--parse-queue
             (not (timerp agent-sidebar--parse-timer)))
    (setq agent-sidebar--parse-timer
          (run-with-idle-timer
           agent-sidebar-parse-idle-delay t
           #'agent-sidebar--parse-tick))))

(defun agent-sidebar--parse-tick ()
  "Parse the next chunk of entries, then redraw affected sidebars."
  (let ((n 0) (parsed nil))
    (while (and agent-sidebar--parse-queue
                (< n agent-sidebar-parse-chunk-size))
      (let ((entry (pop agent-sidebar--parse-queue)))
        (agent-sidebar--ensure-parsed entry)
        (push entry parsed))
      (cl-incf n))
    (when parsed
      (dolist (buf (buffer-list))
        (when (and (buffer-live-p buf)
                   (eq (buffer-local-value 'major-mode buf)
                       'agent-sidebar-mode))
          (with-current-buffer buf
            (agent-sidebar--redraw)))))
    (unless agent-sidebar--parse-queue
      (when (timerp agent-sidebar--parse-timer)
        (cancel-timer agent-sidebar--parse-timer))
      (setq agent-sidebar--parse-timer nil))))

;;; Discovery aggregation

(defun agent-sidebar--all-entries ()
  "Return a flat list of entries from every enabled provider."
  (let (all)
    (dolist (p (agent-sidebar--enabled-providers))
      (let ((lister (plist-get p :list)))
        (when lister
          (setq all (append all (funcall lister))))))
    all))

;;; agent-shell provider

(defun agent-sidebar--agent-shell-transcripts-for-root (root)
  "Return transcript file paths under project ROOT."
  (let ((dir (expand-file-name ".agent-shell/transcripts/" root)))
    (when (file-directory-p dir)
      (ignore-errors
        (directory-files dir t "\\.md\\'" t)))))

(defun agent-sidebar--agent-shell-list ()
  "List all agent-shell transcript entries across known projects."
  (let (entries)
    (dolist (root (agent-sidebar--project-roots))
      (dolist (path (agent-sidebar--agent-shell-transcripts-for-root root))
        (let* ((attrs (file-attributes path))
               (mtime (and attrs
                           (float-time
                            (file-attribute-modification-time attrs)))))
          (push (list :provider 'agent-shell
                      :id path
                      :repo root
                      :agent nil
                      :mtime mtime
                      :extras (list :file path))
                entries))))
    entries))

(defun agent-sidebar--agent-shell-parse (entry)
  "Parse the transcript header for ENTRY, returning a metadata plist."
  (let ((file (plist-get entry :id)))
    (with-temp-buffer
      (condition-case _
          (insert-file-contents file nil 0 8192)
        (error nil))
      (let (agent started session-id model cwd preview)
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\*Agent:\\*\\*[ \t]+\\(.*\\)$" nil t)
          (setq agent (string-trim (match-string 1))))
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\*Started:\\*\\*[ \t]+\\(.*\\)$" nil t)
          (setq started (string-trim (match-string 1))))
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\*Working Directory:\\*\\*[ \t]+\\(.*\\)$" nil t)
          (setq cwd (string-trim (match-string 1))))
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\*Session ID:\\*\\*[ \t]+\\(.*\\)$" nil t)
          (setq session-id (string-trim (match-string 1))))
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\*Model:\\*\\*[ \t]+\\(.*\\)$" nil t)
          (setq model (string-trim (match-string 1))))
        (goto-char (point-min))
        (when (re-search-forward "^## User[^\n]*$" nil t)
          (forward-line 1)
          (while (and (not (eobp))
                      (looking-at-p "^[ \t]*$"))
            (forward-line 1))
          (unless (eobp)
            (let* ((line (buffer-substring-no-properties
                          (point) (line-end-position)))
                   (stripped (if (string-match "\\`>[ \t]*\\(.*\\)\\'" line)
                                 (match-string 1 line)
                               line))
                   (trimmed (string-trim stripped)))
              (unless (string-empty-p trimmed)
                (setq preview trimmed)))))
        (list :agent agent
              :model model
              :timestamp started
              :cwd cwd
              :session-id session-id
              :preview preview)))))

(defun agent-sidebar--live-agent-shell-buffer-for-session (session-id)
  "Return a live agent-shell buffer whose session id equals SESSION-ID."
  (and session-id
       (seq-find (lambda (buf)
                   (with-current-buffer buf
                     (equal session-id
                            (map-nested-elt agent-shell--state
                                            '(:session :id)))))
                 (agent-shell-buffers))))

(defun agent-sidebar--config-for-agent-name (name)
  "Return the agent-shell config whose display name matches NAME, or nil."
  (when (and name (fboundp 'agent-shell--resolved-agent-configs))
    (let ((configs (agent-shell--resolved-agent-configs))
          (name-fold (downcase name)))
      (or (seq-find (lambda (cfg)
                      (or (equal name (map-elt cfg :mode-line-name))
                          (equal name (map-elt cfg :buffer-name))))
                    configs)
          (seq-find (lambda (cfg)
                      (let ((mln (or (map-elt cfg :mode-line-name) ""))
                            (bn (or (map-elt cfg :buffer-name) "")))
                        (or (string-search name-fold (downcase mln))
                            (string-search name-fold (downcase bn))
                            (string-search (downcase mln) name-fold)
                            (string-search (downcase bn) name-fold))))
                    configs)))))

(defun agent-sidebar--agent-shell-visit (entry)
  "Visit an agent-shell ENTRY: resume live buffer, or start a new shell.
When the transcript lacks a `**Session ID:**' header, cross-reference
Claude CLI's own JSONL storage by cwd + start-timestamp to recover the
resumable session id.  As a last resort, hand off to agent-shell's own
session picker via `session-strategy'."
  (let* ((meta (agent-sidebar--ensure-parsed entry))
         (agent-name (plist-get meta :agent))
         (cwd (plist-get meta :cwd))
         (started (plist-get meta :timestamp))
         (session-id (or (plist-get meta :session-id)
                         (agent-sidebar--find-claude-session-for-transcript
                          agent-name cwd started)))
         (session-source (cond
                          ((plist-get meta :session-id) 'from-transcript)
                          (session-id 'from-claude-cli)
                          (t 'none)))
         (live (agent-sidebar--live-agent-shell-buffer-for-session session-id)))
    (cond
     (live
      (agent-sidebar--pop-to live))
     (t
      (let* ((default-directory
              (if (and cwd (file-directory-p cwd))
                  (file-name-as-directory cwd)
                default-directory))
             (matched (agent-sidebar--config-for-agent-name agent-name))
             (auto (and (not matched)
                        (fboundp 'agent-shell--auto-preferred-config)
                        (agent-shell--auto-preferred-config)))
             (config-source (cond (matched 'matched)
                                  (auto 'auto-preferred)
                                  (t 'prompt)))
             (config (or matched
                         auto
                         (agent-shell-select-config
                          :prompt "Start with agent: ")))
             ;; When we don't know the session id, hand off to agent-shell's
             ;; own session picker (ACP `session/list' → `session/resume').
             ;; This is the same flow the user sees from `agent-shell-new-shell'.
             (session-strategy (if session-id 'new 'prompt)))
        (message "agent-sidebar[agent-shell]: file=%s agent=%S session=%S (%s) config=%s (%s) strategy=%s cwd=%s"
                 (file-name-nondirectory (plist-get entry :id))
                 agent-name session-id session-source
                 (map-elt config :identifier) config-source
                 session-strategy default-directory)
        (let ((buf (agent-shell--start
                    :config config
                    :session-id session-id
                    :session-strategy session-strategy
                    :new-session t
                    :no-focus t)))
          (agent-sidebar--pop-to buf)))))))

;;; Cross-reference: agent-shell transcript -> Claude CLI session-id
;;
;; The transcript's `**Session ID:**' header field is often empty on older
;; sessions (Claude Code didn't populate it earlier).  Since the resumable
;; session id lives in Claude CLI's own JSONL files, we can locate the
;; matching JSONL for a transcript by (cwd + start timestamp) and reuse
;; its UUID as the session id.

(defvar agent-sidebar--claude-sessions-by-cwd-cache
  (make-hash-table :test 'equal)
  "Cache of cwd -> list of (uuid . first-timestamp-seconds).")

(defun agent-sidebar--iso-to-seconds (str)
  "Parse timestamp STR to seconds-since-epoch, or nil.
Accepts both agent-shell's local-time format and Claude's ISO UTC form."
  (condition-case _
      (float-time (date-to-time str))
    (error nil)))

(defun agent-sidebar--claude-jsonl-first-timestamp (file)
  "Return the first `timestamp' field found in JSONL FILE, or nil."
  (with-temp-buffer
    (condition-case _
        (insert-file-contents file nil 0 4096)
      (error nil))
    (goto-char (point-min))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol)
          ts)
      (while (and (not ts) (not (eobp)))
        (let* ((line (buffer-substring-no-properties
                      (point) (line-end-position)))
               (obj (condition-case _
                        (json-read-from-string line)
                      (error nil))))
          (when-let* ((t2 (and obj (alist-get 'timestamp obj))))
            (setq ts t2)))
        (forward-line 1))
      ts)))

(defun agent-sidebar--claude-sessions-for-cwd (cwd)
  "Return list of (UUID . SECONDS) for Claude CLI sessions in CWD."
  (let ((cached (gethash cwd agent-sidebar--claude-sessions-by-cwd-cache
                         'not-cached)))
    (if (not (eq cached 'not-cached))
        cached
      (let* ((encoded (agent-sidebar--claude-encode-path cwd))
             (dir (expand-file-name
                   encoded agent-sidebar-claude-cli-projects-dir))
             sessions)
        (when (file-directory-p dir)
          (dolist (f (ignore-errors
                       (directory-files dir t "\\.jsonl\\'" t)))
            (when-let* ((ts (agent-sidebar--claude-jsonl-first-timestamp f))
                        (secs (agent-sidebar--iso-to-seconds ts)))
              (push (cons (file-name-base f) secs) sessions))))
        (puthash cwd sessions
                 agent-sidebar--claude-sessions-by-cwd-cache)
        sessions))))

(defun agent-sidebar--find-claude-session-for-transcript
    (agent-name cwd started-str)
  "Return a Claude CLI session UUID matching a transcript, or nil.
Matches when AGENT-NAME resolves to Claude, CWD has Claude sessions,
and one of them started within 60 seconds of STARTED-STR."
  (when (and agent-name cwd started-str
             (string-match-p "\\`Claude" agent-name))
    (when-let* ((tx-secs (agent-sidebar--iso-to-seconds started-str))
                (sessions (agent-sidebar--claude-sessions-for-cwd cwd)))
      (let (best-uuid best-delta)
        (dolist (s sessions)
          (let ((delta (abs (- tx-secs (cdr s)))))
            (when (or (null best-delta) (< delta best-delta))
              (setq best-uuid (car s)
                    best-delta delta))))
        (and best-uuid best-delta (< best-delta 60) best-uuid)))))

;;; claude-cli provider

(defun agent-sidebar--claude-encode-path (path)
  "Encode PATH the way Claude CLI encodes project cwd into a directory name."
  (replace-regexp-in-string
   "[/.]" "-"
   (directory-file-name (expand-file-name path))))

(defvar agent-sidebar--claude-project-decode-cache nil
  "Alist ((ENCODED . DECODED) ...) built at list-time.")

(defun agent-sidebar--claude-decode-project-dir (encoded)
  "Return the actual project root for a Claude project ENCODED name.
Falls back to a trivial decoding when no known root matches."
  (or (cdr (assoc encoded agent-sidebar--claude-project-decode-cache))
      (let ((decoded
             (concat "/"
                     (replace-regexp-in-string
                      "^-" "" encoded))))
        (setq decoded (replace-regexp-in-string "-" "/" decoded))
        decoded)))

(defun agent-sidebar--claude-rebuild-decode-cache ()
  "Rebuild `agent-sidebar--claude-project-decode-cache' from known roots."
  (setq agent-sidebar--claude-project-decode-cache
        (mapcar (lambda (r)
                  (cons (agent-sidebar--claude-encode-path r) r))
                (agent-sidebar--project-roots))))

(defun agent-sidebar--claude-list ()
  "List all Claude CLI session entries from ~/.claude/projects/*/*.jsonl."
  (when (file-directory-p agent-sidebar-claude-cli-projects-dir)
    (agent-sidebar--claude-rebuild-decode-cache)
    (let (entries)
      (dolist (proj-dir (ignore-errors
                          (directory-files
                           agent-sidebar-claude-cli-projects-dir
                           t "\\`[^.]" t)))
        (when (file-directory-p proj-dir)
          (let ((encoded (file-name-nondirectory
                          (directory-file-name proj-dir))))
            (dolist (path (ignore-errors
                            (directory-files
                             proj-dir t "\\.jsonl\\'" t)))
              (let* ((attrs (file-attributes path))
                     (mtime (and attrs
                                 (float-time
                                  (file-attribute-modification-time attrs))))
                     (uuid (file-name-base path))
                     (repo (agent-sidebar--claude-decode-project-dir encoded)))
                (push (list :provider 'claude-cli
                            :id path
                            :repo (file-name-as-directory repo)
                            :agent "Claude"
                            :mtime mtime
                            :extras (list :session-id uuid
                                          :encoded-dir encoded))
                      entries))))))
      entries)))

(defun agent-sidebar--claude-parse (entry)
  "Parse the first non-meta user message from a Claude CLI JSONL ENTRY."
  (let ((file (plist-get entry :id))
        (session-id (plist-get (plist-get entry :extras) :session-id))
        (bytes-to-read 65536)
        cwd model timestamp preview)
    (with-temp-buffer
      (condition-case _
          (insert-file-contents file nil 0 bytes-to-read)
        (error nil))
      (goto-char (point-min))
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol))
        (while (and (not (eobp))
                    (or (null preview) (null cwd) (null timestamp)))
          (let ((line-end (line-end-position))
                obj)
            (setq obj
                  (condition-case _
                      (json-read-from-string
                       (buffer-substring-no-properties (point) line-end))
                    (error nil)))
            (when obj
              (unless timestamp
                (when-let* ((ts (alist-get 'timestamp obj)))
                  (setq timestamp ts)))
              (unless cwd
                (when-let* ((c (alist-get 'cwd obj)))
                  (setq cwd c)))
              (unless model
                (when-let* ((msg (alist-get 'message obj))
                            (m (alist-get 'model msg)))
                  (setq model m)))
              (unless preview
                (when (and (equal (alist-get 'type obj) "user")
                           (not (eq (alist-get 'isMeta obj) t))
                           (not (alist-get 'isMeta obj)))
                  (when-let* ((msg (alist-get 'message obj))
                              (content (alist-get 'content msg))
                              (text (cond
                                     ((stringp content) content)
                                     ((and (listp content) content)
                                      (or (alist-get 'text (car content))
                                          ""))
                                     (t ""))))
                    (let ((trimmed (string-trim text)))
                      (unless (or (string-empty-p trimmed)
                                  (string-prefix-p "<local-command-caveat>"
                                                   trimmed)
                                  (string-prefix-p "<command-name>"
                                                   trimmed)
                                  (string-prefix-p "<command-message>"
                                                   trimmed)
                                  (string-prefix-p "<command-args>"
                                                   trimmed)
                                  (string-prefix-p "<local-command-stdout>"
                                                   trimmed)
                                  (string-prefix-p "<local-command-stderr>"
                                                   trimmed))
                        (setq preview
                              (car (split-string trimmed "\n" t))))))))
            (forward-line 1)))))
    (list :agent "Claude"
          :model model
          :timestamp timestamp
          :cwd cwd
          :session-id session-id
          :preview preview))))

(defun agent-sidebar--claude-visit (entry)
  "Resume a Claude CLI ENTRY in a terminal via `claude --resume <uuid>'."
  (let* ((meta (agent-sidebar--ensure-parsed entry))
         (session-id (or (plist-get meta :session-id)
                         (plist-get (plist-get entry :extras) :session-id)))
         (cwd (or (plist-get meta :cwd)
                  (plist-get entry :repo)
                  default-directory))
         (args (append agent-sidebar-claude-cli-resume-args
                       (list session-id)))
         (name (format "*claude-resume: %s*" (substring session-id 0 8))))
    (message "agent-sidebar[claude-cli]: %s %s (cwd=%s)"
             agent-sidebar-claude-cli-command
             (string-join args " ") cwd)
    (agent-sidebar--launch-terminal
     name agent-sidebar-claude-cli-command args cwd)))

;;; Provider registration

(agent-sidebar-register-provider
 (list :id 'agent-shell
       :name "agent-shell"
       :list #'agent-sidebar--agent-shell-list
       :parse #'agent-sidebar--agent-shell-parse
       :visit #'agent-sidebar--agent-shell-visit))

(agent-sidebar-register-provider
 (list :id 'claude-cli
       :name "Claude CLI"
       :list #'agent-sidebar--claude-list
       :parse #'agent-sidebar--claude-parse
       :visit #'agent-sidebar--claude-visit))

;;; Entry accessors (consult cache when available)

(defun agent-sidebar--entry-field (entry key)
  "Return KEY from ENTRY, preferring parsed metadata when cached."
  (let ((meta (agent-sidebar--cached-meta entry)))
    (or (and meta (plist-get meta key))
        (plist-get entry key))))

(defun agent-sidebar--entry-agent (entry)
  "Return the agent/model label for ENTRY (parsed or fallback)."
  (or (agent-sidebar--entry-field entry :agent)
      (plist-get entry :agent)))

(defun agent-sidebar--entry-preview (entry)
  "Return the parsed preview for ENTRY, or nil if not yet parsed."
  (let ((meta (agent-sidebar--cached-meta entry)))
    (and meta (plist-get meta :preview))))

(defun agent-sidebar--entry-timestamp (entry)
  "Return the display timestamp for ENTRY as a short date."
  (let* ((meta (agent-sidebar--cached-meta entry))
         (ts (and meta (plist-get meta :timestamp))))
    (cond
     ((and ts (string-match
               "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)[T ]\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
               ts))
      (format "%s-%s %s:%s"
              (match-string 2 ts) (match-string 3 ts)
              (match-string 4 ts) (match-string 5 ts)))
     (t
      (let ((mtime (plist-get entry :mtime)))
        (if mtime
            (format-time-string "%m-%d %H:%M" (seconds-to-time mtime))
          "??-?? ??:??"))))))

;;; Grouping engine

(defun agent-sidebar--entry-date-key (entry)
  "Return a YYYY-MM-DD string for grouping ENTRY by date.
Uses the parsed timestamp when available, else file mtime."
  (let* ((meta (agent-sidebar--cached-meta entry))
         (ts (and meta (plist-get meta :timestamp))))
    (cond
     ((and ts (string-match
               "\\`\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
               ts))
      (match-string 1 ts))
     (t
      (let ((mtime (plist-get entry :mtime)))
        (if mtime
            (format-time-string "%Y-%m-%d" (seconds-to-time mtime))
          "unknown"))))))

(defun agent-sidebar--group-key (entry level)
  "Return the group key for ENTRY at LEVEL (a symbol)."
  (pcase level
    ('package (plist-get entry :provider))
    ('repo    (or (plist-get entry :repo) "(no repo)"))
    ('model   (or (agent-sidebar--entry-agent entry) "?"))
    ('date    (agent-sidebar--entry-date-key entry))
    (_ nil)))

(defun agent-sidebar--build-tree (entries levels)
  "Group ENTRIES recursively by LEVELS, returning a nested tree.
Node shape: (:level LEVEL :key KEY :breadcrumb (KEYS...)
             :entries LIST :children NODES)."
  (agent-sidebar--build-tree-1 entries levels '()))

(defun agent-sidebar--build-tree-1 (entries levels breadcrumb)
  "See `agent-sidebar--build-tree'.  BREADCRUMB is the path of keys so far."
  (if (null levels)
      entries
    (let* ((level (car levels))
           (rest (cdr levels))
           (groups (make-hash-table :test 'equal))
           (order nil))
      (dolist (e entries)
        (let ((k (agent-sidebar--group-key e level)))
          (unless (gethash k groups)
            (push k order))
          (push e (gethash k groups))))
      (let ((nodes
             (mapcar
              (lambda (k)
                (let* ((es (nreverse (gethash k groups)))
                       (crumb (append breadcrumb (list (cons level k)))))
                  (list :level level
                        :key k
                        :breadcrumb crumb
                        :entries es
                        :children
                        (agent-sidebar--build-tree-1 es rest crumb))))
              (nreverse order))))
        (sort nodes
              (lambda (a b)
                (let ((ma (agent-sidebar--node-latest-mtime a))
                      (mb (agent-sidebar--node-latest-mtime b)))
                  (cond
                   ((and (null ma) (null mb))
                    (string< (format "%s" (plist-get a :key))
                             (format "%s" (plist-get b :key))))
                   ((null ma) nil)
                   ((null mb) t)
                   (t (> ma mb))))))))))

(defun agent-sidebar--node-latest-mtime (node)
  "Return the newest mtime among entries in NODE."
  (let ((mtimes (delq nil
                      (mapcar (lambda (e) (plist-get e :mtime))
                              (plist-get node :entries)))))
    (and mtimes (apply #'max mtimes))))

;;; Rendering

(defconst agent-sidebar--placeholder
  (propertize "…" 'face 'agent-sidebar-placeholder-face))

(defun agent-sidebar--display-name (level key entries)
  "Return the display string for a group header at LEVEL with KEY."
  (pcase level
    ('package
     (let ((p (alist-get key agent-sidebar-providers)))
       (or (and p (plist-get p :name)) (symbol-name key))))
    ('repo
     (let* ((roots (delete-dups
                    (delq nil
                          (mapcar (lambda (e) (plist-get e :repo)) entries))))
            (all-roots (or roots (list key)))
            (names (agent-sidebar--disambiguated-names all-roots)))
       (or (gethash key names)
           (file-name-nondirectory (directory-file-name key)))))
    ('model (or key "?"))
    ('date (or key "unknown"))
    (_ (format "%s" key))))

(defun agent-sidebar--face-for-level (level)
  "Return the face used for a header at LEVEL."
  (pcase level
    ('package 'agent-sidebar-package-face)
    ('repo    'agent-sidebar-project-face)
    ('model   'agent-sidebar-agent-face)
    ('date    'agent-sidebar-date-face)
    (_        'agent-sidebar-project-face)))

(defun agent-sidebar--render-group-header (node depth entries-at-node)
  "Insert a group header line for NODE at DEPTH.
ENTRIES-AT-NODE is the flat list of entries below this node."
  (let* ((level (plist-get node :level))
         (key (plist-get node :key))
         (crumb (plist-get node :breadcrumb))
         (collapsed (gethash crumb agent-sidebar--collapsed))
         (arrow (if collapsed "▸" "▾"))
         (name (agent-sidebar--display-name level key entries-at-node))
         (indent (make-string (* 2 depth) ?\s))
         (label (format "%s%s %s (%d)"
                        indent arrow name (length entries-at-node)))
         (start (point)))
    (insert (propertize label 'face (agent-sidebar--face-for-level level)))
    (add-text-properties
     start (point)
     `(agent-sidebar-group ,crumb
       agent-sidebar-collapsed ,collapsed))
    (insert "\n")))

(defun agent-sidebar--render-entry-row (entry depth mark)
  "Insert one chat row for ENTRY at DEPTH with optional MARK."
  (let* ((parsed (not (null (agent-sidebar--cached-meta entry))))
         (date-str (agent-sidebar--entry-timestamp entry))
         (date (if parsed
                   (propertize date-str 'face 'agent-sidebar-date-face)
                 (propertize date-str
                             'face 'agent-sidebar-placeholder-face)))
         (preview (agent-sidebar--entry-preview entry))
         (mark-str (if (eq mark 'delete)
                       (propertize "D" 'face 'agent-sidebar-mark-face)
                     " "))
         (indent (make-string (+ 2 (* 2 depth)) ?\s))
         (start (point)))
    (insert indent mark-str " " date
            (cond
             (preview (concat ": " preview))
             (parsed "")
             (t (concat ": " agent-sidebar--placeholder))))
    (add-text-properties
     start (point)
     `(agent-sidebar-entry ,entry
       help-echo ,(agent-sidebar--entry-help entry)))
    (insert "\n")))

(defun agent-sidebar--entry-help (entry)
  "Return a tooltip string for ENTRY."
  (let ((meta (agent-sidebar--cached-meta entry))
        (id (plist-get entry :id)))
    (if meta
        (format "%s\nProvider: %s\nAgent: %s\nSession: %s\nCwd: %s%s"
                id
                (plist-get entry :provider)
                (or (plist-get meta :agent) "?")
                (or (plist-get meta :session-id) "(none)")
                (or (plist-get meta :cwd) "?")
                (if (plist-get meta :preview)
                    (concat "\n\n" (plist-get meta :preview))
                  ""))
      id)))

(defun agent-sidebar--render-tree (nodes depth)
  "Render NODES at DEPTH recursively into the current buffer."
  (dolist (node nodes)
    (let* ((entries (plist-get node :entries))
           (children (plist-get node :children))
           (crumb (plist-get node :breadcrumb))
           (collapsed (gethash crumb agent-sidebar--collapsed)))
      (agent-sidebar--render-group-header node depth entries)
      (unless collapsed
        (cond
         ;; children is a list of nodes (deeper grouping): recurse
         ((and (consp children)
               (consp (car children))
               (plist-get (car children) :level))
          (agent-sidebar--render-tree children (1+ depth)))
         ;; children is a list of entries (leaf level): render each
         (t
          (dolist (e children)
            (agent-sidebar--render-entry-row
             e (1+ depth)
             (gethash (plist-get e :id) agent-sidebar--marks)))))))))

(defun agent-sidebar--redraw ()
  "Redraw the current sidebar buffer, preserving point when possible."
  (when (derived-mode-p 'agent-sidebar-mode)
    (let* ((prev-entry-id
            (let ((e (get-text-property (point) 'agent-sidebar-entry)))
              (and e (plist-get e :id))))
           (prev-group (get-text-property (point) 'agent-sidebar-group))
           (prev-line (line-number-at-pos))
           (inhibit-read-only t)
           (entries (agent-sidebar--all-entries)))
      (erase-buffer)
      (unless agent-sidebar--marks
        (setq agent-sidebar--marks (make-hash-table :test 'equal)))
      (unless agent-sidebar--collapsed
        (setq agent-sidebar--collapsed (make-hash-table :test 'equal)))
      (cond
       ((null entries)
        (insert (propertize "  (no chats found)\n"
                            'face 'agent-sidebar-placeholder-face)))
       ((null agent-sidebar-grouping)
        (let ((sorted (sort (copy-sequence entries)
                            (lambda (a b)
                              (let ((ma (plist-get a :mtime))
                                    (mb (plist-get b :mtime)))
                                (cond
                                 ((and (null ma) (null mb)) nil)
                                 ((null ma) nil)
                                 ((null mb) t)
                                 (t (> ma mb))))))))
          (dolist (e sorted)
            (agent-sidebar--render-entry-row
             e 0 (gethash (plist-get e :id) agent-sidebar--marks)))))
       (t
        (agent-sidebar--render-tree
         (agent-sidebar--build-tree entries agent-sidebar-grouping)
         0)))
      (goto-char (point-min))
      (cond
       (prev-entry-id
        (let ((found nil))
          (while (and (not found) (not (eobp)))
            (let ((e (get-text-property (point) 'agent-sidebar-entry)))
              (when (and e (equal prev-entry-id (plist-get e :id)))
                (setq found t)))
            (unless found (forward-line 1)))
          (unless found (goto-char (point-min))
                  (forward-line (1- prev-line)))))
       (prev-group
        (let ((found nil))
          (while (and (not found) (not (eobp)))
            (when (equal prev-group
                         (get-text-property (point) 'agent-sidebar-group))
              (setq found t))
            (unless found (forward-line 1)))
          (unless found (goto-char (point-min)))))
       (t (forward-line (1- prev-line)))))))

;;; Sidebar mode + keymap

(defvar agent-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agent-sidebar-visit)
    (define-key map (kbd "o") #'agent-sidebar-open-transcript)
    (define-key map (kbd "g") #'agent-sidebar-refresh)
    (define-key map (kbd "q") #'agent-sidebar-hide-sidebar)
    (define-key map (kbd "n") #'agent-sidebar-next-line)
    (define-key map (kbd "p") #'agent-sidebar-previous-line)
    (define-key map (kbd "d") #'agent-sidebar-mark-delete)
    (define-key map (kbd "u") #'agent-sidebar-unmark)
    (define-key map (kbd "U") #'agent-sidebar-unmark-all)
    (define-key map (kbd "x") #'agent-sidebar-execute)
    (define-key map (kbd "TAB") #'agent-sidebar-toggle-group)
    (define-key map (kbd "<tab>") #'agent-sidebar-toggle-group)
    (define-key map [mouse-2] #'agent-sidebar-mouse-visit)
    map)
  "Keymap for `agent-sidebar-mode'.")

(define-derived-mode agent-sidebar-mode special-mode "AgentChats"
  "Major mode for the AI agent chat sidebar."
  :group 'agent-sidebar
  (setq truncate-lines t
        buffer-read-only t
        window-size-fixed agent-sidebar-window-fixed
        cursor-in-non-selected-windows nil)
  (setq-local agent-sidebar--marks (make-hash-table :test 'equal))
  (setq-local agent-sidebar--collapsed (make-hash-table :test 'equal))
  (when agent-sidebar-refresh-timer
    (setq-local agent-sidebar--refresh-timer-object
                (run-with-idle-timer
                 agent-sidebar-refresh-timer t
                 (lambda ()
                   (when (buffer-live-p (get-buffer agent-sidebar-name))
                     (with-current-buffer (get-buffer agent-sidebar-name)
                       (agent-sidebar-refresh)))))))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when (timerp agent-sidebar--refresh-timer-object)
                (cancel-timer agent-sidebar--refresh-timer-object)))
            nil t))

;;; Commands: navigation

(defun agent-sidebar-next-line (&optional n)
  "Move to the next actionable row (entry or group header)."
  (interactive "p")
  (let ((n (or n 1)))
    (dotimes (_ (abs n))
      (forward-line (if (> n 0) 1 -1))
      (while (and (not (eobp))
                  (not (bobp))
                  (not (get-text-property (point) 'agent-sidebar-entry))
                  (not (get-text-property (point) 'agent-sidebar-group)))
        (forward-line (if (> n 0) 1 -1))))))

(defun agent-sidebar-previous-line (&optional n)
  "Move to the previous actionable row."
  (interactive "p")
  (agent-sidebar-next-line (- (or n 1))))

;;; Commands: activation

(defun agent-sidebar--pop-to (buffer)
  "Pop to BUFFER in the MRU or next window, per user setting."
  (let ((win (if agent-sidebar-open-file-in-most-recently-used-window
                 (get-mru-window nil nil t)
               (next-window))))
    (if win
        (progn (select-window win)
               (switch-to-buffer buffer))
      (pop-to-buffer buffer))))

(defun agent-sidebar-visit ()
  "Activate the row at point.
Group headers toggle their fold; chat rows dispatch to the entry's
provider `:visit' function."
  (interactive)
  (cond
   ((get-text-property (point) 'agent-sidebar-group)
    (agent-sidebar-toggle-group))
   ((get-text-property (point) 'agent-sidebar-entry)
    (let* ((entry (get-text-property (point) 'agent-sidebar-entry))
           (provider (agent-sidebar--entry-provider entry))
           (visitor (plist-get provider :visit)))
      (unless visitor
        (user-error "Provider %s has no :visit function"
                    (plist-get entry :provider)))
      (funcall visitor entry)))
   (t (user-error "Nothing at point"))))

(defun agent-sidebar-mouse-visit (event)
  "Handle mouse click EVENT on a row."
  (interactive "e")
  (let ((posn (event-end event)))
    (with-current-buffer (window-buffer (posn-window posn))
      (goto-char (posn-point posn))
      (agent-sidebar-visit))))

(defun agent-sidebar-open-transcript ()
  "Open the source file for the entry at point (read-only view)."
  (interactive)
  (let ((entry (get-text-property (point) 'agent-sidebar-entry)))
    (unless entry (user-error "No entry at point"))
    (agent-sidebar--pop-to (find-file-noselect (plist-get entry :id)))))

(defun agent-sidebar-toggle-group ()
  "Fold or unfold the group at point."
  (interactive)
  (let ((crumb (get-text-property (point) 'agent-sidebar-group)))
    (unless crumb (user-error "Not on a group header"))
    (if (gethash crumb agent-sidebar--collapsed)
        (remhash crumb agent-sidebar--collapsed)
      (puthash crumb t agent-sidebar--collapsed))
    (agent-sidebar--redraw)))

;;; Commands: marks

(defun agent-sidebar-mark-delete ()
  "Mark the entry at point for deletion."
  (interactive)
  (let ((entry (get-text-property (point) 'agent-sidebar-entry)))
    (unless entry (user-error "No entry at point"))
    (puthash (plist-get entry :id) 'delete agent-sidebar--marks)
    (agent-sidebar--redraw)
    (agent-sidebar-next-line 1)))

(defun agent-sidebar-unmark ()
  "Remove any mark from the entry at point."
  (interactive)
  (let ((entry (get-text-property (point) 'agent-sidebar-entry)))
    (unless entry (user-error "No entry at point"))
    (remhash (plist-get entry :id) agent-sidebar--marks)
    (agent-sidebar--redraw)
    (agent-sidebar-next-line 1)))

(defun agent-sidebar-unmark-all ()
  "Remove all deletion marks."
  (interactive)
  (clrhash agent-sidebar--marks)
  (agent-sidebar--redraw))

(defun agent-sidebar-execute ()
  "Delete all entries marked for deletion (removes the source file)."
  (interactive)
  (let (ids)
    (maphash (lambda (id mark)
               (when (eq mark 'delete) (push id ids)))
             agent-sidebar--marks)
    (cond
     ((null ids) (message "No marks to execute"))
     ((yes-or-no-p (format "Delete %d chat file(s)? " (length ids)))
      (dolist (id ids)
        (when (file-exists-p id)
          (delete-file id))
        (remhash id agent-sidebar--parse-cache)
        (remhash id agent-sidebar--marks))
      (agent-sidebar-refresh)
      (message "Deleted %d file(s)" (length ids))))))

;;; Commands: refresh

(defun agent-sidebar-refresh ()
  "Rescan providers and redraw."
  (interactive)
  (let ((entries (agent-sidebar--all-entries)))
    (agent-sidebar--queue-uncached entries)
    (agent-sidebar--redraw)
    (agent-sidebar--start-parse-timer)))

;;; Sidebar window commands

(defun agent-sidebar--sidebar-window (&optional _frame)
  "Return the sidebar window if visible in the current frame."
  (seq-find
   (lambda (w)
     (with-current-buffer (window-buffer w)
       (derived-mode-p 'agent-sidebar-mode)))
   (window-list)))

(defun agent-sidebar-showing-sidebar-p ()
  "Non-nil if the sidebar is visible in the selected frame."
  (agent-sidebar--sidebar-window))

(defun agent-sidebar--get-or-create-buffer ()
  "Return the sidebar buffer, creating and initializing it if needed."
  (let ((existing (get-buffer agent-sidebar-name)))
    (or existing
        (let ((buf (generate-new-buffer agent-sidebar-name)))
          (with-current-buffer buf
            (agent-sidebar-mode)
            (agent-sidebar-refresh))
          buf))))

(defun agent-sidebar--set-width (width)
  "Set the sidebar window width to WIDTH."
  (unless (one-window-p)
    (let ((window-size-fixed)
          (w (max width window-min-width)))
      (cond
       ((> (window-width) w)
        (shrink-window-horizontally (- (window-width) w)))
       ((< (window-width) w)
        (enlarge-window-horizontally (- w (window-width))))))))

;;;###autoload
(defun agent-sidebar-show-sidebar ()
  "Show the agent chat sidebar."
  (interactive)
  (let ((buffer (agent-sidebar--get-or-create-buffer)))
    (display-buffer-in-side-window buffer agent-sidebar-display-alist)
    (let ((window (get-buffer-window buffer)))
      (when window
        (set-window-dedicated-p window t)
        (when agent-sidebar-no-delete-other-windows
          (set-window-parameter window 'no-delete-other-windows t))
        (when agent-sidebar-resize-on-open
          (with-selected-window window
            (let ((window-size-fixed))
              (agent-sidebar--set-width agent-sidebar-width))))))))

;;;###autoload
(defun agent-sidebar-hide-sidebar ()
  "Hide the agent chat sidebar in the selected frame."
  (interactive)
  (when-let* ((win (agent-sidebar--sidebar-window)))
    (delete-window win)))

;;;###autoload
(defun agent-sidebar-toggle-sidebar ()
  "Toggle the agent chat sidebar."
  (interactive)
  (if (agent-sidebar-showing-sidebar-p)
      (agent-sidebar-hide-sidebar)
    (agent-sidebar-show-sidebar)
    (when agent-sidebar-pop-to-sidebar-on-toggle-open
      (when-let* ((win (agent-sidebar--sidebar-window)))
        (select-window win)))))

;;;###autoload
(defun agent-sidebar-jump-to-sidebar ()
  "Jump to the sidebar window, showing it first if hidden."
  (interactive)
  (if-let* ((win (agent-sidebar--sidebar-window)))
      (select-window win)
    (call-interactively #'agent-sidebar-toggle-sidebar)))

;;; Evil bindings

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal agent-sidebar-mode-map
      (kbd "RET") #'agent-sidebar-visit
      (kbd "o")   #'agent-sidebar-open-transcript
      (kbd "gr")  #'agent-sidebar-refresh
      (kbd "gg")  #'evil-goto-first-line
      (kbd "G")   #'evil-goto-line
      (kbd "j")   #'agent-sidebar-next-line
      (kbd "k")   #'agent-sidebar-previous-line
      (kbd "d")   #'agent-sidebar-mark-delete
      (kbd "u")   #'agent-sidebar-unmark
      (kbd "U")   #'agent-sidebar-unmark-all
      (kbd "x")   #'agent-sidebar-execute
      (kbd "TAB") #'agent-sidebar-toggle-group
      (kbd "^")   #'agent-sidebar-toggle-group
      (kbd "-")   #'agent-sidebar-toggle-group
      (kbd "q")   #'agent-sidebar-hide-sidebar
      (kbd "ZZ")  #'agent-sidebar-hide-sidebar
      (kbd "ZQ")  #'agent-sidebar-hide-sidebar))
  (when (fboundp 'evil-make-overriding-map)
    (evil-make-overriding-map agent-sidebar-mode-map 'normal)))

(provide 'agent-sidebar)
;;; agent-sidebar.el ends here
