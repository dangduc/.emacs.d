;;; agent-sidebar.el --- Browse agent-shell, Claude, and Codex chats -*- lexical-binding: t; coding: utf-8; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Keywords: agent-shell, claude, codex, tools
;; Package-Requires: ((emacs "29.1") (agent-shell "0.74.3") (project "0.9"))

;;; Commentary:
;; Groups file-backed conversations by provider, repository, model, and date.
;; This file owns discovery, metadata caching, idle parsing, and sidebar windows.
;; Entry point: M-x agent-sidebar-toggle-sidebar.
;; RET resumes; o reads; / filters; G changes grouping; N starts a new session.
;; Codex CLI sessions use ghostel; A resumes the selected Codex chat in agent-shell.

;;; Code:


(require 'cl-lib)

(require 'generator)

(require 'text-property-search)

(require 'map)

(require 'seq)

(require 'subr-x)

(require 'project)

(require 'view)

(require 'agent-shell)

(declare-function projectile-known-projects "projectile")

(defvar projectile-known-projects)

(declare-function evil-define-key* "evil-core")

(declare-function evil-make-overriding-map "evil-core")

(declare-function evil-goto-first-line "evil-commands")

(declare-function evil-goto-line "evil-commands")

(require 'json)

(require 'term)

(declare-function ghostel-exec "ghostel")

(declare-function vterm "vterm")

(defvar vterm-shell)

;;; Customization and faces

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

(defcustom agent-sidebar-enabled-providers '(agent-shell claude-cli codex-cli)
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

(defcustom agent-sidebar-codex-cli-command "codex"
  "Executable used for Codex CLI sessions in ghostel."
  :type 'string)

(defcustom agent-sidebar-codex-home
  (expand-file-name (or (getenv "CODEX_HOME") "~/.codex/"))
  "Codex data directory used for discovery and sidebar-launched sessions.
Active rollouts are discovered under sessions/YYYY/MM/DD.  Archived
sessions are excluded.  This directory is passed to child processes as
CODEX_HOME; the parent Emacs environment is unchanged."
  :type 'directory)

(defcustom agent-sidebar-terminal-function nil
  "Function to run PROGRAM with ARGS in a terminal at DIRECTORY.
Called as (NAME PROGRAM ARGS DIRECTORY).  When nil, `agent-sidebar'
tries ghostel, then vterm, then `term'."
  :type '(choice (const :tag "Auto (ghostel > vterm > term)" nil)
                 function))

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

(defface agent-sidebar-model-face
  '((t :inherit font-lock-comment-face))
  "Face for the model column.")

(defcustom agent-sidebar-include-remote-projects nil
  "Whether discovery scans remote projects.
Remote directory and file operations can wait for a connection."
  :type 'boolean)

;;; State

(defconst agent-sidebar--cache-schema 4
  "Bump when the parsed header format changes to invalidate old entries.")

(defvar-local agent-sidebar--parse-cache nil
  "Maps this buffer's paths to schema, file signature, and header plists.
The header plist has :agent :started :session-id :model :cwd :preview.")

(defvar-local agent-sidebar--parse-timer nil
  "Active idle timer for chunked header parsing.")

(defvar-local agent-sidebar--parse-queue nil
  "List of transcript paths awaiting header parse.")

(defvar-local agent-sidebar--parse-generation nil
  "Identity of the current parse queue, invalidated by refresh or cleanup.")

(defvar-local agent-sidebar--marks nil
  "Hash table mapping transcript path -> mark symbol (e.g. `delete').")

(defvar-local agent-sidebar--collapsed nil
  "Hash table of collapsed project roots.")

(defvar-local agent-sidebar--refresh-timer-object nil
  "Per-buffer idle timer for auto-refresh.")

(defvar-local agent-sidebar--groups nil
  "Project and transcript snapshot from the last refresh.")

(defvar-local agent-sidebar--file-info nil
  "Hash table of transcript paths to file attributes from the last refresh.")

(defvar-local agent-sidebar--context-roots nil
  "Directories from which this sidebar was opened.")

(defvar-local agent-sidebar--filter ""
  "Case-insensitive literal filter for transcript metadata and paths.")

(defvar-local agent-sidebar--pending-count 0
  "Number of headers still queued.")

(defvar-local agent-sidebar--parse-retry-file nil
  "Queue head that has already failed once in this refresh.")

(defvar-local agent-sidebar--render-iterator nil
  "Iterator constructing the next coherent metadata snapshot.")

(defvar-local agent-sidebar--render-buffer nil
  "Staging buffer for the next metadata snapshot.")

(defvar-local agent-sidebar--render-dirty nil
  "Non-nil when metadata changed since the current render began.")

(defvar-local agent-sidebar--render-error nil
  "Rendering error, cleared by a manual redraw or refresh.")

(defvar-local agent-sidebar--project-names nil
  "Disambiguated project names from the last synchronous redraw.")

(defvar agent-sidebar--timer-buffers nil
  "Sidebar buffers whose timers must restart after input.")

(defvar agent-sidebar--resetting-idle nil
  "Non-nil while resetting timer delays for a new idle period.")

(defvar-local agent-sidebar--collect-function nil
  "Optional discovery function returning groups and setting file attributes.")

(defvar-local agent-sidebar--parse-file-function nil
  "Optional function that returns metadata for a queued file path.")

(defvar-local agent-sidebar--render-contents-function nil
  "Optional function that inserts the current view without scanning files.")

(defvar-local agent-sidebar--render-snapshot-function nil
  "Optional iterator factory accepting output buffer, groups, and names.")

(defvar-local agent-sidebar--owned-file-p-function nil
  "Optional predicate for additional files owned by live session buffers.")

(defvar-local agent-sidebar--row-identity-properties
  '(agent-sidebar-file agent-sidebar-agent agent-sidebar-project)
  "Text properties identifying actionable rows in this sidebar.")

(defconst agent-sidebar--placeholder
  (propertize "…" 'face 'agent-sidebar-placeholder-face))

(defvar agent-sidebar-providers nil
  "Alist of provider ids and plists with :name, :list, :parse, and :visit.
A lister returns entries with a regular file path as :id, a :provider id,
and optional :repo, :agent, and :extras fields.  A parser returns metadata.")

(defvar-local agent-sidebar--entries nil "Current entry snapshots, keyed by file path.")

(defvar-local agent-sidebar--entry-list nil "Current entries in newest-first order.")

(defvar-local agent-sidebar--claude-sessions-by-cwd-cache nil
  "Directory-signature cache for session-id recovery.")

(defvar-local agent-sidebar--terminal-session nil "Provider session identity owned by this terminal.")

(defvar-local agent-sidebar--terminal-file nil "Transcript owned by this terminal.")

(defvar-local agent-sidebar--agent-shell-file nil
  "Native Codex rollout owned by this agent-shell buffer.")

(defvar-local agent-sidebar--codex-session-home nil
  "Codex data directory used to start this agent-shell buffer.")

(defconst agent-sidebar--preview-limit 200
  "Maximum characters stored in a display preview, including its ellipsis.
Sidebar filters search this bounded preview; transcripts retain the full text.")

;;; Discovery, metadata, timers, and windows

(defun agent-sidebar--regular-file-attributes-p (attrs)
  "Return non-nil when ATTRS describe a regular file, excluding symlinks."
  (and attrs
       (null (file-attribute-type attrs))
       (string-prefix-p "-" (file-attribute-modes attrs))))

(defun agent-sidebar--project-roots ()
  "Return normalized roots from project lists and sidebar context."
  (let ((roots (append
                (when (fboundp 'project-known-project-roots)
                  (project-known-project-roots))
                (when (bound-and-true-p projectile-known-projects)
                  projectile-known-projects)
                agent-sidebar-extra-project-roots
                agent-sidebar--context-roots
                (mapcar (lambda (buffer)
                          (buffer-local-value 'default-directory buffer))
                        (agent-shell-buffers)))))
    (delete-dups
     (delq nil
           (mapcar (lambda (root)
                     (when (and (stringp root) (not (string-empty-p root))
                                (or agent-sidebar-include-remote-projects
                                    (not (file-remote-p root))))
                       (file-name-as-directory (expand-file-name root))))
                   roots)))))

(defun agent-sidebar--transcripts-for-root (root)
  "Return regular transcript files under ROOT, newest first.
Record attributes once for sorting, cache lookup, and display."
  (condition-case nil
      (let (paths)
        (dolist (entry (directory-files-and-attributes
                       (expand-file-name ".agent-shell/transcripts/" root)
                       t "\\.md\\'" t))
          (let ((file (car entry)) (attrs (cdr entry)))
            (when (agent-sidebar--regular-file-attributes-p attrs)
              (puthash file attrs agent-sidebar--file-info)
              (push file paths))))
        (sort paths #'agent-sidebar--newer-p))
    (file-error nil)))

(defun agent-sidebar--mtime (file)
  "Return FILE's modification time from the discovery snapshot."
  (when-let* ((attrs (and agent-sidebar--file-info
                         (gethash file agent-sidebar--file-info))))
    (file-attribute-modification-time attrs)))

(defun agent-sidebar--newer-p (a b)
  "Return non-nil when A precedes B by modification time and then path."
  (let ((ma (agent-sidebar--mtime a))
        (mb (agent-sidebar--mtime b)))
    (cond ((equal ma mb) (string< a b))
          ((null ma) nil)
          ((null mb) t)
          (t (time-less-p mb ma)))))

(defun agent-sidebar--parse-header-from-file (file)
  "Read at most 8192 bytes of FILE and return transcript metadata.
Only fields before the header separator or first message are metadata.
Return an :error field when FILE is not regular, cannot be read, or its
metadata section extends past the byte limit.  Keep complete fields."
  (with-temp-buffer
    (condition-case err
        (let ((attrs (file-attributes file)) after)
          ;; Discovery is only a snapshot: a path may have become a FIFO or
          ;; another special file before its queued parse or activation.
          (unless (agent-sidebar--regular-file-attributes-p attrs)
            (signal 'file-error (list "Not a regular transcript file" file)))
          (insert-file-contents file nil 0 8192)
          (setq after (file-attributes file))
          (unless (agent-sidebar--regular-file-attributes-p after)
            (signal 'file-error (list "Transcript changed while reading; refresh and retry" file)))
          (let* ((truncated (or (> (file-attribute-size attrs) 8192)
                                (> (file-attribute-size after) 8192)))
                 ;; The artificial end of a bounded read is not a line end.
                 (complete-end (if truncated
                                   (save-excursion
                                     (goto-char (point-max))
                                     (line-beginning-position))
                                 (point-max)))
                 (boundary (save-excursion
                             (goto-char (point-min))
                             (when (re-search-forward
                                    "^\\(?:---[ \t]*$\\|## \\)" complete-end t)
                               (match-beginning 0))))
                 (end (or boundary complete-end))
                 header)
            (dolist (field '(("Agent" . :agent) ("Started" . :started)
                             ("Session ID" . :session-id) ("Model" . :model)
                             ("Working Directory" . :cwd)))
              (goto-char (point-min))
              (when (re-search-forward
                     (concat "^\\*\\*" (car field) ":\\*\\*"
                             (if (eq (cdr field) :cwd) "[ \t]?" "[ \t]*")
                             "\\(.*\\)$") end t)
                (let ((value (match-string-no-properties 1)))
                  ;; agent-shell writes one formatting space before CWD.
                  ;; Any further whitespace belongs to the directory name.
                  (unless (eq (cdr field) :cwd)
                    (setq value (string-trim value)))
                  (unless (string-empty-p value)
                    (setq header (plist-put header (cdr field) value))))))
            (when (and truncated (not boundary))
              (setq header (plist-put header :error
                                      "Transcript metadata exceeds the 8192-byte read limit")))
            (goto-char end)
            (when (re-search-forward "^## User\\(?:[ \t].*\\)?$" nil t)
              (forward-line 1)
              (while (and (not (eobp)) (looking-at-p "^[ \t]*$"))
                (forward-line 1))
              (unless (or (eobp) (looking-at-p "^\\(?:## \\|---[ \t]*$\\)"))
                (setq header
                      (plist-put
                       header :preview
                       (string-trim
                        (replace-regexp-in-string
                         "\\`>[ \t]*" ""
                         (buffer-substring-no-properties (point) (line-end-position))))))))
            (or header '(:preview nil))))
      (file-error (list :error (error-message-string err))))))

(defun agent-sidebar--signature (attrs)
  "Return the cache signature for file ATTRS."
  (when attrs
    (list (file-attribute-modification-time attrs)
          (file-attribute-size attrs)
          (file-attribute-inode-number attrs))))

(defun agent-sidebar--cached-header (file &optional attrs)
  "Return FILE's cached header when its signature matches ATTRS.
Use the discovery snapshot when ATTRS is nil.  Do not access the disk."
  (let ((entry (and agent-sidebar--parse-cache
                    (gethash file agent-sidebar--parse-cache)))
        (signature (agent-sidebar--signature
                    (or attrs (and agent-sidebar--file-info
                                   (gethash file agent-sidebar--file-info))))))
    (when (and signature
               (equal (plist-get entry :schema) agent-sidebar--cache-schema)
               (equal (plist-get entry :signature) signature))
      (plist-get entry :header))))

(defun agent-sidebar--ensure-header (file)
  "Read FILE's metadata when changed or when a previous read failed."
  (unless agent-sidebar--parse-cache
    (setq agent-sidebar--parse-cache (make-hash-table :test 'equal)))
  (let* ((owner (current-buffer))
         (generation agent-sidebar--parse-generation)
         (cache agent-sidebar--parse-cache)
         (entry (gethash file cache))
         (attrs (condition-case nil (file-attributes file) (file-error nil)))
         (cached (and (agent-sidebar--regular-file-attributes-p attrs)
                      (agent-sidebar--cached-header file attrs))))
    (or (and cached (not (plist-get cached :error)) cached)
        (let ((header (funcall (or agent-sidebar--parse-file-function
                                   #'agent-sidebar--parse-header-from-file)
                               file)))
          ;; File hooks can refresh, replace the mode, or parse this file again.
          ;; A completed obsolete read must not overwrite that newer state.
          (when (and (buffer-live-p owner) (eq (current-buffer) owner)
                     (eq generation agent-sidebar--parse-generation)
                     (eq cache agent-sidebar--parse-cache)
                     (eq entry (gethash file cache)))
            (puthash file (list :schema agent-sidebar--cache-schema
                                :signature (agent-sidebar--signature attrs)
                                :header header)
                     cache)
            (when agent-sidebar--file-info
              (puthash file attrs agent-sidebar--file-info)))
          header))))

(defun agent-sidebar--queue-uncached (paths)
  "Queue PATHS missing successful cached metadata for this refresh."
  (setq agent-sidebar--parse-generation (make-symbol "sidebar-parse")
        agent-sidebar--parse-queue
        (seq-remove (lambda (file)
                      (let ((header (agent-sidebar--cached-header file)))
                        (and header (not (plist-get header :error)))))
                    paths)
        agent-sidebar--pending-count (length agent-sidebar--parse-queue)
        agent-sidebar--parse-retry-file nil))

(defun agent-sidebar--idle-timer (delay function)
  "Call FUNCTION with the current buffer after another DELAY idle seconds.
Add elapsed idle time so successive chunks also run during continuous idle."
  (run-with-idle-timer
   (time-add (if agent-sidebar--resetting-idle 0
               (or (current-idle-time) 0))
             (max 0.01 delay)) nil
   function (current-buffer)))

(defun agent-sidebar--start-parse-timer ()
  "Schedule the next chunk while parsing or rendering remains."
  (when (and (or agent-sidebar--parse-queue
                 (and (not agent-sidebar--render-error)
                      (or agent-sidebar--render-iterator
                          agent-sidebar--render-dirty)))
             (not (timerp agent-sidebar--parse-timer)))
    (setq agent-sidebar--parse-timer
          (agent-sidebar--idle-timer
           agent-sidebar-parse-idle-delay #'agent-sidebar--parse-tick))))

(defun agent-sidebar--current-parse-p (owner generation)
  "Return non-nil if OWNER still holds the sidebar work in GENERATION."
  (and (buffer-live-p owner) (eq (current-buffer) owner)
       (derived-mode-p 'agent-sidebar-mode)
       (eq generation agent-sidebar--parse-generation)))

(defun agent-sidebar--parse-tick (&optional buffer)
  "Parse and render bounded portions of BUFFER without scanning projects."
  (when (buffer-live-p (or buffer (current-buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (when (derived-mode-p 'agent-sidebar-mode)
        (when (timerp agent-sidebar--parse-timer)
          (cancel-timer agent-sidebar--parse-timer))
        (setq agent-sidebar--parse-timer nil)
        (let ((owner (current-buffer))
              (generation agent-sidebar--parse-generation))
          (unwind-protect
              (let ((n 0) retry)
                (while (and (agent-sidebar--current-parse-p owner generation)
                            agent-sidebar--parse-queue (not retry)
                            (< n (max 1 agent-sidebar-parse-chunk-size))
                            (not (input-pending-p)))
                  (let* ((queue agent-sidebar--parse-queue)
                         (file (car queue)))
                    (condition-case err
                        (agent-sidebar--ensure-header file)
                      (error
                       (when (and (agent-sidebar--current-parse-p owner generation)
                                  (eq queue agent-sidebar--parse-queue))
                         (if (equal file agent-sidebar--parse-retry-file)
                             (puthash
                              file
                              (list :schema agent-sidebar--cache-schema
                                    :signature (agent-sidebar--signature
                                                (gethash file agent-sidebar--file-info))
                                    :header (list :error (error-message-string err)))
                              agent-sidebar--parse-cache)
                           (setq agent-sidebar--parse-retry-file file
                                 retry t)))))
                    ;; The head remains pending if a hook replaced or advanced
                    ;; the queue.  A quit also retains it and rearms work below.
                    (when (and (not retry)
                               (agent-sidebar--current-parse-p owner generation)
                               (eq queue agent-sidebar--parse-queue))
                      (pop agent-sidebar--parse-queue)
                      (setq agent-sidebar--parse-retry-file nil
                            agent-sidebar--pending-count
                            (max 0 (1- agent-sidebar--pending-count))
                            agent-sidebar--render-dirty t)))
                  (cl-incf n))
                (when (agent-sidebar--current-parse-p owner generation)
                  (agent-sidebar--render-tick)
                  (agent-sidebar--update-header-line)))
            (when (buffer-live-p owner)
              (with-current-buffer owner
                (when (derived-mode-p 'agent-sidebar-mode)
                  (agent-sidebar--start-parse-timer))))))))))

(defun agent-sidebar--reset-idle-timers ()
  "Restart pending timers after input anywhere in Emacs."
  (let ((agent-sidebar--resetting-idle t))
    (dolist (buffer agent-sidebar--timer-buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (dolist (timer (list agent-sidebar--parse-timer
                              agent-sidebar--refresh-timer-object))
            (when (timerp timer) (cancel-timer timer)))
          (setq agent-sidebar--parse-timer nil
                agent-sidebar--refresh-timer-object nil)
          (agent-sidebar--start-parse-timer)
          (agent-sidebar--schedule-refresh))))))

(defun agent-sidebar--schedule-refresh ()
  "Schedule another refresh for this buffer when auto-refresh is enabled."
  (when (timerp agent-sidebar--refresh-timer-object)
    (cancel-timer agent-sidebar--refresh-timer-object))
  (setq agent-sidebar--refresh-timer-object nil)
  (when (and (numberp agent-sidebar-refresh-timer)
             (> agent-sidebar-refresh-timer 0))
    (setq agent-sidebar--refresh-timer-object
          (agent-sidebar--idle-timer
           agent-sidebar-refresh-timer #'agent-sidebar--refresh-tick))))

(defun agent-sidebar--refresh-tick (buffer)
  "Refresh BUFFER when visible, then schedule its next refresh."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq agent-sidebar--refresh-timer-object nil)
      (when (derived-mode-p 'agent-sidebar-mode)
        (unwind-protect
            (when (and (numberp agent-sidebar-refresh-timer)
                       (> agent-sidebar-refresh-timer 0)
                       (get-buffer-window buffer t))
              (agent-sidebar-refresh))
          (agent-sidebar--schedule-refresh))))))

(defun agent-sidebar--cleanup ()
  "Cancel this buffer's timers and pending work."
  (setq agent-sidebar--parse-generation nil)
  (agent-sidebar--cancel-render)
  (setq agent-sidebar--timer-buffers
        (delq (current-buffer) agent-sidebar--timer-buffers))
  (unless agent-sidebar--timer-buffers
    (remove-hook 'post-command-hook #'agent-sidebar--reset-idle-timers))
  (dolist (timer (list agent-sidebar--parse-timer
                       agent-sidebar--refresh-timer-object))
    (when (timerp timer) (cancel-timer timer)))
  (setq agent-sidebar--parse-timer nil
        agent-sidebar--refresh-timer-object nil
        agent-sidebar--parse-queue nil
        agent-sidebar--pending-count 0
        agent-sidebar--parse-retry-file nil))

(defun agent-sidebar--config-names (config)
  "Return nonempty display names from CONFIG."
  (seq-filter (lambda (name) (and (stringp name) (not (string-empty-p name))))
              (list (map-elt config :mode-line-name) (map-elt config :buffer-name))))

(defun agent-sidebar--configs-for-agent-name (name)
  "Return configurations matching NAME, preferring exact names to substrings.
Compare names case-insensitively.
Never match an empty configuration name."
  (when (and (stringp name) (not (string-empty-p name)))
    (let* ((configs (agent-shell--resolved-agent-configs))
           (folded (downcase name))
           (exact (seq-filter
                   (lambda (config)
                     (member folded (mapcar #'downcase
                                            (agent-sidebar--config-names config))))
                   configs))
           (matches (or exact
                        (seq-filter
                         (lambda (config)
                           (seq-some
                            (lambda (candidate)
                              (let ((candidate (downcase candidate)))
                                (or (string-search folded candidate)
                                    (string-search candidate folded))))
                            (agent-sidebar--config-names config)))
                         configs))))
      matches)))

(defun agent-sidebar--config-for-agent-name (name)
  "Return an unambiguous configuration matching NAME."
  (let ((matches (agent-sidebar--configs-for-agent-name name)))
    (when (= (length matches) 1) (car matches))))

(defun agent-sidebar--live-buffer-for-session (session-id &optional agent-name cwd config)
  "Find an existing or connecting SESSION-ID for AGENT-NAME in CWD.
When CONFIG is supplied, match that resolved configuration instead of
AGENT-NAME.  An active session ID takes precedence over a pending resume ID.
Without CONFIG, refuse ambiguous matches across different configurations."
  (when (and (stringp session-id) (not (string-empty-p session-id)))
    (let ((matches
           (seq-filter
            (lambda (buffer)
              (with-current-buffer buffer
                (let ((active-id (map-nested-elt agent-shell--state '(:session :id))))
                  (and (equal session-id
                              (if (and (stringp active-id) (not (string-empty-p active-id)))
                                  active-id
                                (map-elt agent-shell--state :resume-session-id)))
                       (if config
                           (equal config (map-elt agent-shell--state :agent-config))
                         (or (null agent-name)
                             (member (downcase agent-name)
                                     (mapcar #'downcase
                                             (agent-sidebar--config-names
                                              (map-elt agent-shell--state :agent-config))))))
                       (or (null cwd)
                           (equal (file-name-as-directory (expand-file-name cwd))
                                  (file-name-as-directory (expand-file-name default-directory))))))))
            (agent-shell-buffers))))
      (when (or config
                (= 1 (length (delete-dups
                              (mapcar (lambda (buffer)
                                        (map-elt (buffer-local-value 'agent-shell--state buffer)
                                                 :agent-config))
                                      matches)))))
        (car matches)))))

(defun agent-sidebar--root-for-file (file)
  "Return FILE's discovered project root, or infer its transcript layout."
  (or (car (seq-find (lambda (group) (member file (cdr group)))
                    agent-sidebar--groups))
      (when (string-match "\\`\\(.*?/\\)\\.agent-shell/transcripts/[^/]+\\'" file)
        (match-string 1 file))
      (user-error "Cannot determine project for %s" file)))

(defun agent-sidebar--working-directory (file header)
  "Resolve FILE's recorded working directory from HEADER.
Use its project root only when the transcript has no recorded directory."
  (let* ((root (agent-sidebar--root-for-file file))
         (cwd (plist-get header :cwd))
         (directory (file-name-as-directory (expand-file-name (or cwd root) root))))
    (unless (file-directory-p directory)
      (user-error "Transcript directory no longer exists: %s" directory))
    directory))

(defun agent-sidebar--select-config (name)
  "Resolve NAME or ask which agent to use when no unique match exists."
  (or (agent-sidebar--config-for-agent-name name)
      (and (null name) (agent-shell--auto-preferred-config))
      (agent-shell-select-config
       :prompt (if name (format "Agent for %s: " name) "Start with agent: "))
      (user-error "No agent selected")))

(defun agent-sidebar--start (directory config &optional session-id strategy)
  "Start CONFIG in DIRECTORY, optionally resuming SESSION-ID using STRATEGY."
  (let* ((default-directory directory)
         ;; agent-shell otherwise promotes a subdirectory to its project root.
         (agent-shell-cwd-function (lambda () directory))
         (buffer (agent-shell--start :config config :session-id session-id
                                     :session-strategy (or strategy 'new) :new-session t :no-focus t)))
    (with-current-buffer buffer
      ;; ACP initialization continues after `agent-shell--start' returns.
      (setq-local agent-shell-cwd-function (lambda () directory)))
    buffer))

(defun agent-sidebar--disambiguated-names (roots)
  "Return a hash table mapping each ROOT to a unique display name.
Names use the shortest trailing path suffix that is unique across ROOTS.
For example, given \"/a/31/fzfa\" and \"/b/32/fzfa\", the results are
\"31/fzfa\" and \"32/fzfa\" respectively."
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
                          (reverse (seq-take parts d)) "/")))
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
                 (if parts (string-join (reverse (seq-take parts d)) "/") "/")
                 result)))
    result))

(defun agent-sidebar--row-location (position)
  "Record POSITION by row identity, line number, and column."
  (save-excursion
    (goto-char position)
    (list (seq-some
           (lambda (property)
             (when-let* ((value (get-text-property (line-beginning-position) property)))
               (cons property value)))
           agent-sidebar--row-identity-properties)
          (line-number-at-pos) (current-column))))

(defun agent-sidebar--restore-location (location)
  "Move to the row in LOCATION, or its former line if the row disappeared."
  (goto-char (point-min))
  (let ((key (car location)))
    (when key
      (goto-char (or (let ((match (text-property-search-forward
                                    (car key) (cdr key) #'equal)))
                       (and match (prop-match-beginning match)))
                     (point-max))))
    (when (or (null key) (eobp))
      (goto-char (point-min))
      (forward-line (1- (cadr location)))
      (when (and (eobp) (not (bobp))) (forward-line -1))))
  (move-to-column (min (or (caddr location) 0)
                       (- (line-end-position) (line-beginning-position)))))

(defun agent-sidebar--redraw ()
  "Redraw the current sidebar buffer, preserving point on the same file/project."
  (when (derived-mode-p 'agent-sidebar-mode)
    (agent-sidebar--cancel-render)
    (setq agent-sidebar--render-error nil)
    (let* ((location (agent-sidebar--row-location (point)))
           (views (mapcar
                   (lambda (window)
                     (list window
                           (agent-sidebar--row-location (window-start window))
                           (agent-sidebar--row-location (window-point window))))
                   (get-buffer-window-list (current-buffer) nil t)))
           (inhibit-read-only t))
      (erase-buffer)
      (unless agent-sidebar--marks
        (setq agent-sidebar--marks (make-hash-table :test 'equal)))
      (unless agent-sidebar--collapsed
        (setq agent-sidebar--collapsed (make-hash-table :test 'equal)))
      (agent-sidebar--render-contents)
      (when (= (point-min) (point-max))
        (insert "  (no matching transcripts)\n"))
      (agent-sidebar--update-header-line)
      (dolist (view views)
        (when (window-live-p (car view))
          (agent-sidebar--restore-location (cadr view))
          (set-window-start (car view) (line-beginning-position) t)
          (agent-sidebar--restore-location (caddr view))
          (set-window-point (car view) (point))))
      (agent-sidebar--restore-location location))))

(defun agent-sidebar--update-header-line ()
  "Update controls and work status without traversing the parse queue."
  (setq header-line-format
        (format " RET resume  o read  TAB fold  / filter  N new  g refresh%s%s%s"
                (if (string-empty-p agent-sidebar--filter) ""
                  (concat " | " (replace-regexp-in-string "%" "%%" agent-sidebar--filter)))
                (if agent-sidebar--parse-queue
                    (format " | %d pending" agent-sidebar--pending-count) "")
                (cond (agent-sidebar--render-error " | redraw failed; g retries")
                      (agent-sidebar--render-iterator " | updating view")
                      (t "")))))

(defun agent-sidebar--cancel-render ()
  "Discard any unpublished snapshot and pending render request."
  (setq agent-sidebar--render-iterator nil
        agent-sidebar--render-dirty nil)
  (when (buffer-live-p agent-sidebar--render-buffer)
    (kill-buffer agent-sidebar--render-buffer))
  (setq agent-sidebar--render-buffer nil))

(defun agent-sidebar--append-item (queue item)
  "Append ITEM to the (HEAD . TAIL) QUEUE in constant time."
  (let ((cell (list item)))
    (if (cdr queue) (setcdr (cdr queue) cell) (setcar queue cell))
    (setcdr queue cell)))

(defun agent-sidebar--publish-render ()
  "Publish the completed snapshot, preserving each current window view."
  (let ((inhibit-quit t)
        (location (agent-sidebar--row-location (point)))
        (views (mapcar
                (lambda (window)
                  (list window
                        (agent-sidebar--row-location (window-start window))
                        (agent-sidebar--row-location (window-point window))))
                (get-buffer-window-list (current-buffer) nil t))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-buffer-substring agent-sidebar--render-buffer))
    (dolist (view views)
      (when (window-live-p (car view))
        (agent-sidebar--restore-location (cadr view))
        (set-window-start (car view) (line-beginning-position) t)
        (agent-sidebar--restore-location (caddr view))
        (set-window-point (car view) (point))))
    (agent-sidebar--restore-location location))
  (kill-buffer agent-sidebar--render-buffer)
  (setq agent-sidebar--render-buffer nil
        agent-sidebar--render-iterator nil))

(defun agent-sidebar--render-tick ()
  "Advance at most 300 path or row operations, coalescing parse updates.
Publication copies a completed buffer and restores row locations."
  (unless agent-sidebar--render-error
    (condition-case err
        (progn
          (when (and agent-sidebar--render-dirty
                     (not agent-sidebar--render-iterator))
            (setq agent-sidebar--render-dirty nil
                  agent-sidebar--render-buffer
                  (generate-new-buffer " *agent-sidebar render*")
                  agent-sidebar--render-iterator
                  (funcall (or agent-sidebar--render-snapshot-function
                               #'agent-sidebar--render-snapshot)
                           agent-sidebar--render-buffer agent-sidebar--groups
                           agent-sidebar--project-names)))
          (let ((n 0))
            (while (and agent-sidebar--render-iterator (< n 300)
                        (not (input-pending-p)))
              (condition-case nil
                  (iter-next agent-sidebar--render-iterator)
                (iter-end-of-sequence (agent-sidebar--publish-render)))
              (cl-incf n))))
      (error
       (agent-sidebar--cancel-render)
       (setq agent-sidebar--render-error (error-message-string err))
       (message "Sidebar redraw failed: %s" agent-sidebar--render-error))
      (quit
       (agent-sidebar--cancel-render)
       (setq agent-sidebar--render-dirty t)
       (signal (car err) (cdr err)))))
  (when agent-sidebar--render-error
    (setq agent-sidebar--render-dirty nil)))

(defun agent-sidebar-next-line (&optional n)
  "Move N actionable rows without leaving the last row."
  (interactive "p")
  (let ((step (if (< (or n 1) 0) -1 1)))
    (dotimes (_ (abs (or n 1)))
      (let ((old (point)))
        (forward-line step)
        (unless (seq-some (lambda (property) (get-text-property (point) property))
                          agent-sidebar--row-identity-properties)
          (goto-char old)))))
  (beginning-of-line))

(defun agent-sidebar-previous-line (&optional n)
  "Move N actionable rows backward."
  (interactive "p")
  (agent-sidebar-next-line (- (or n 1))))

(defun agent-sidebar--pop-to (buffer)
  "Display BUFFER in an ordinary window and select it."
  (let* ((windows (seq-filter
                   (lambda (window)
                     (and (not (window-dedicated-p window))
                          (not (window-parameter window 'window-side))))
                   (window-list nil 'no-minibuffer)))
         (window (if agent-sidebar-open-file-in-most-recently-used-window
                     (car (sort windows (lambda (a b) (> (window-use-time a) (window-use-time b)))))
                   (car windows))))
    (if window
        (progn (set-window-buffer window buffer) (select-window window))
      (pop-to-buffer buffer '((display-buffer-pop-up-window display-buffer-pop-up-frame))))))

(defun agent-sidebar--visit-header (file header &optional strategy)
  "Visit FILE using validated HEADER and optional startup STRATEGY."
  (when (plist-get header :error) (user-error "%s" (plist-get header :error)))
  (let* ((session-id (plist-get header :session-id))
         (name (plist-get header :agent))
         (directory (agent-sidebar--working-directory file header))
         (existing (and name
                        (agent-sidebar--live-buffer-for-session
                         session-id name directory)))
         ;; A removed configuration must not prevent access to its live shell.
         ;; Configured ambiguous matches still require explicit selection.
         (unconfigured (and existing
                            (null (agent-sidebar--configs-for-agent-name name))))
         (config (unless unconfigured (agent-sidebar--select-config name)))
         (live (if unconfigured existing
                 (agent-sidebar--live-buffer-for-session session-id name directory config))))
    (agent-sidebar--pop-to
     (or live (if strategy
                  (agent-sidebar--start directory config session-id strategy)
                (agent-sidebar--start directory config session-id))))))

(defun agent-sidebar-open-transcript ()
  "Open the transcript at point in View mode."
  (interactive)
  (let ((file (get-text-property (line-beginning-position) 'agent-sidebar-file)))
    (unless file (user-error "No transcript at point"))
    (unless (file-readable-p file) (user-error "Cannot read transcript: %s" file))
    (unless (agent-sidebar--regular-file-attributes-p (file-attributes file))
      (user-error "Not a regular transcript file: %s" file))
    (let ((buffer (find-file-noselect file)))
      (with-current-buffer buffer (view-mode 1))
      (agent-sidebar--pop-to buffer))))

(defun agent-sidebar-set-filter (text)
  "Filter rows by literal TEXT.  With a prefix argument, clear the filter."
  (interactive (list (if current-prefix-arg ""
                       (read-string "Filter transcripts: " agent-sidebar--filter))))
  (setq agent-sidebar--filter (string-trim text))
  (agent-sidebar--redraw))

(defun agent-sidebar-mark-delete ()
  "Mark the transcript at point for deletion."
  (interactive)
  (let ((file (get-text-property (line-beginning-position) 'agent-sidebar-file)))
    (unless file (user-error "No transcript at point"))
    (puthash file 'delete agent-sidebar--marks)
    (agent-sidebar--redraw)
    (agent-sidebar-next-line 1)))

(defun agent-sidebar-unmark ()
  "Remove any mark from the transcript at point."
  (interactive)
  (let ((file (get-text-property (line-beginning-position) 'agent-sidebar-file)))
    (unless file (user-error "No transcript at point"))
    (remhash file agent-sidebar--marks)
    (agent-sidebar--redraw)
    (agent-sidebar-next-line 1)))

(defun agent-sidebar-unmark-all ()
  "Remove all deletion marks."
  (interactive)
  (clrhash agent-sidebar--marks)
  (agent-sidebar--redraw))

(defun agent-sidebar-execute ()
  "Delete marked transcripts after confirmation, retaining failed marks.
Refuse deletion of transcripts owned by existing agent-shell buffers."
  (interactive)
  (let (files failures (deleted 0))
    (maphash (lambda (path mark) (when (eq mark 'delete) (push path files)))
             agent-sidebar--marks)
    (cond
     ((null files) (message "No marks to execute"))
     ((yes-or-no-p (format "Delete %d transcript file(s)? " (length files)))
      (dolist (file files)
        (condition-case err
            (progn
              (when (seq-some
                     (lambda (buffer)
                       (let ((owned (buffer-local-value 'agent-shell--transcript-file buffer)))
                         (and (stringp owned) (not (string-empty-p owned))
                              (or (equal file owned) (file-equal-p file owned)))))
                     (agent-shell-buffers))
                (user-error "Transcript belongs to an open agent-shell: %s" file))
              (when (and agent-sidebar--owned-file-p-function
                         (funcall agent-sidebar--owned-file-p-function file))
                (user-error "Transcript belongs to an open session: %s" file))
              (when (file-exists-p file) (delete-file file delete-by-moving-to-trash))
              (cl-incf deleted)
              (remhash file agent-sidebar--parse-cache)
              (remhash file agent-sidebar--marks))
          (file-error (push (error-message-string err) failures))
          (user-error (push (error-message-string err) failures))))
      (agent-sidebar-refresh)
      (if failures
          (message "Deleted %d. Retained %d mark(s): %s"
                   deleted (length failures) (string-join (nreverse failures) "; "))
        (message "Deleted %d transcript(s)" deleted))))))

(defun agent-sidebar-refresh ()
  "Rescan transcripts once and schedule uncached metadata for parsing."
  (interactive)
  (unless (derived-mode-p 'agent-sidebar-mode)
    (user-error "Not in an agent-shell sidebar"))
  ;; Publish discovery only after it succeeds, preserving pending work on error.
  (let* ((snapshot
          (let ((agent-sidebar--file-info nil))
            (list (funcall (or agent-sidebar--collect-function
                               #'agent-sidebar--collect))
                  agent-sidebar--file-info))))
    (when (timerp agent-sidebar--parse-timer)
      (cancel-timer agent-sidebar--parse-timer))
    (setq agent-sidebar--parse-timer nil
          agent-sidebar--groups (car snapshot)
          agent-sidebar--file-info (cadr snapshot)))
  (agent-sidebar--queue-uncached
   (apply #'append (mapcar #'cdr agent-sidebar--groups)))
  (agent-sidebar--redraw)
  (agent-sidebar--start-parse-timer))

(defun agent-sidebar--sidebar-window (&optional frame)
  "Return a sidebar window in FRAME, or the selected frame when nil."
  (seq-find (lambda (window)
              (and (window-parameter window 'window-side)
                   (with-current-buffer (window-buffer window)
                     (derived-mode-p 'agent-sidebar-mode))))
            (window-list frame 'no-minibuffer)))

(defun agent-sidebar-showing-sidebar-p ()
  "Non-nil if the sidebar is visible in the selected frame."
  (agent-sidebar--sidebar-window))

(defun agent-sidebar--get-or-create-buffer ()
  "Return an initialized sidebar buffer without overwriting another buffer."
  (let ((buffer (get-buffer-create agent-sidebar-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-sidebar-mode)
        (unless (= (buffer-size) 0)
          (user-error "Buffer %s is already in use" agent-sidebar-name))
        (agent-sidebar-mode)))
    buffer))

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

(defun agent-sidebar-show-sidebar ()
  "Show and refresh the sidebar, including the current directory and project."
  (interactive)
  (let* ((directory default-directory)
         (project (unless (file-remote-p directory) (project-current nil)))
         (roots (delq nil (list directory (and project (project-root project)))))
         (buffer (agent-sidebar--get-or-create-buffer))
         (window (with-current-buffer buffer
                   ;; An ordinary view of this buffer must permit the side split.
                   (let ((window-size-fixed nil))
                     (display-buffer-in-side-window
                      buffer (append agent-sidebar-display-alist
                                     `((window-width . ,agent-sidebar-width))))))))
    (with-current-buffer buffer
      (setq agent-sidebar--context-roots
            (delete-dups (append roots agent-sidebar--context-roots)))
      (agent-sidebar-refresh))
    (when window
      (set-window-dedicated-p window t)
      (set-window-parameter window 'no-delete-other-windows
                            agent-sidebar-no-delete-other-windows)
      (when agent-sidebar-resize-on-open
        (with-selected-window window
          (condition-case nil
              (agent-sidebar--set-width agent-sidebar-width)
            (error nil)))))))

(defun agent-sidebar-hide-sidebar ()
  "Hide the agent-shell chat sidebar in the selected frame."
  (interactive)
  (when-let* ((win (agent-sidebar--sidebar-window)))
    (delete-window win)))

(defun agent-sidebar-jump-to-sidebar ()
  "Select the sidebar window, showing it first if hidden."
  (interactive)
  (unless (agent-sidebar-showing-sidebar-p)
    (agent-sidebar-show-sidebar))
  (when-let* ((window (agent-sidebar--sidebar-window)))
    (select-window window)))

;;; Providers, grouping, and commands

(defun agent-sidebar-register-provider (provider)
  "Register PROVIDER, a plist with a symbolic :id and callable operations."
  (unless (and (symbolp (plist-get provider :id)) (plist-get provider :id)
               (stringp (plist-get provider :name))
               (cl-every (lambda (key) (functionp (plist-get provider key)))
                         '(:list :parse :visit)))
    (user-error "Provider needs an id, name, lister, parser, and visit function"))
  (setf (alist-get (plist-get provider :id) agent-sidebar-providers) provider))

(defun agent-sidebar--enabled-providers ()
  "Return enabled providers in configured order."
  (mapcar (lambda (id) (or (alist-get id agent-sidebar-providers)
                           (user-error "Unknown sidebar provider: %s" id)))
          (delete-dups (copy-sequence agent-sidebar-enabled-providers))))

(defun agent-sidebar--entry-provider (entry)
  "Return the provider registered for ENTRY."
  (or (alist-get (plist-get entry :provider) agent-sidebar-providers)
      (user-error "Provider is no longer registered: %s" (plist-get entry :provider))))

(defun agent-sidebar--agent-shell-list ()
  "Discover regular agent-shell transcripts using the shared file snapshot."
  (let ((agent-sidebar--file-info (make-hash-table :test 'equal)) entries)
    (dolist (root (agent-sidebar--project-roots))
      (dolist (file (agent-sidebar--transcripts-for-root root))
        (push (list :provider 'agent-shell :id file :repo root
                    :attrs (gethash file agent-sidebar--file-info)) entries)))
    entries))

(defun agent-sidebar--claude-encode-path (path)
  "Return Claude's project directory key for PATH.  This encoding is lossy."
  (replace-regexp-in-string "[^[:alnum:]-]" "-"
                            (directory-file-name (expand-file-name path))))

(defun agent-sidebar--claude-decode-project-dir (encoded)
  "Return a unique known directory matching ENCODED, or nil.
Hyphens cannot be decoded into directory separators without other evidence."
  (let ((matches (seq-filter
                  (lambda (root) (equal encoded (agent-sidebar--claude-encode-path root)))
                  (agent-sidebar--project-roots))))
    (when (= (length matches) 1) (car matches))))

(defun agent-sidebar--claude-files (directory)
  "Return (FILE . ATTRIBUTES) pairs for regular JSONL files in DIRECTORY."
  (condition-case nil
      (seq-filter (lambda (item) (agent-sidebar--regular-file-attributes-p (cdr item)))
                  (directory-files-and-attributes directory t "\\.jsonl\\'" t))
    (file-error nil)))

(defun agent-sidebar--claude-list ()
  "Discover top-level Claude CLI session files in the configured projects
directory."
  (let ((roots (agent-sidebar--project-roots))
        (decoded (make-hash-table :test 'equal)) entries)
    (dolist (root roots)
      (push root (gethash (agent-sidebar--claude-encode-path root) decoded)))
    (dolist (directory (condition-case nil
                           (directory-files agent-sidebar-claude-cli-projects-dir t "\\`[^.]")
                         (file-error nil)))
      (when (and (file-directory-p directory) (not (file-symlink-p directory)))
        (let* ((known (gethash (file-name-nondirectory directory) decoded))
               (repo (and (= (length known) 1) (car known))))
          (dolist (pair (agent-sidebar--claude-files directory))
            (push (list :provider 'claude-cli :id (car pair) :repo repo :agent "Claude"
                        :attrs (cdr pair)
                        :extras (list :session-id (file-name-base (car pair)))) entries)))))
    entries))

(defun agent-sidebar--collect ()
  "Collect provider entries once, committing state only after successful
discovery."
  (let ((table (make-hash-table :test 'equal))
        (info (make-hash-table :test 'equal)) entries)
    (dolist (provider (agent-sidebar--enabled-providers))
      (dolist (original (funcall (plist-get provider :list)))
        (let* ((entry (copy-tree original)) (file (plist-get entry :id))
               (attrs (and (stringp file)
                           (or (plist-get entry :attrs)
                               (condition-case nil (file-attributes file) (file-error nil))))))
          (when (and (stringp file) (file-name-absolute-p file)
                     (or agent-sidebar-include-remote-projects (not (file-remote-p file)))
                     (agent-sidebar--regular-file-attributes-p attrs))
            (setq entry (plist-put entry :provider (plist-get provider :id)))
            (when (and (gethash file table)
                       (not (eq (plist-get (gethash file table) :provider)
                                (plist-get entry :provider))))
              (user-error "Providers returned the same transcript: %s" file))
            (unless (gethash file table)
              (setq entry (plist-put entry :mtime (float-time (file-attribute-modification-time attrs))))
              (puthash file attrs info)
              (puthash file entry table)
              (push entry entries))))))
    (let ((agent-sidebar--file-info info))
      (setq entries (sort entries (lambda (a b) (agent-sidebar--newer-p
						 (plist-get a :id) (plist-get b :id))))))
    ;; Registration changes must not reuse metadata produced by another parser.
    (dolist (entry entries)
      (let* ((file (plist-get entry :id))
             (header (plist-get (gethash file agent-sidebar--parse-cache) :header))
             (provider (agent-sidebar--entry-provider entry)))
        (unless (and (eq (plist-get header :provider) (plist-get entry :provider))
                     (eq (plist-get header :parser) (plist-get provider :parse)))
          (remhash file agent-sidebar--parse-cache))))
    (setq agent-sidebar--entries table agent-sidebar--entry-list entries
          agent-sidebar--file-info info)
    ;; The shared queue consumes paths.  Tree grouping belongs to the view.
    (when entries (list (cons nil (mapcar (lambda (entry) (plist-get entry :id)) entries))))))

(defun agent-sidebar--all-entries ()
  "Return the current discovery snapshot without scanning directories."
  agent-sidebar--entry-list)

(defun agent-sidebar--cached-meta (entry)
  "Return ENTRY's metadata, including a frozen nil while a snapshot is
rendered."
  (if (plist-member entry :meta) (plist-get entry :meta)
    (agent-sidebar--cached-header (plist-get entry :id))))

(defun agent-sidebar--bounded-preview (preview)
  "Return PREVIEW limited to `agent-sidebar--preview-limit' characters."
  (if (and (stringp preview) (> (length preview) agent-sidebar--preview-limit))
      (concat (substring preview 0 (1- agent-sidebar--preview-limit)) "…")
    preview))

(defun agent-sidebar--parse-file (file)
  "Dispatch FILE through its provider, bounding and annotating its metadata."
  (let* ((entry (gethash file agent-sidebar--entries))
         (provider (and entry (agent-sidebar--entry-provider entry))))
    (unless entry (user-error "Transcript is no longer in the sidebar: %s" file))
    (let ((meta (copy-sequence (funcall (plist-get provider :parse) entry))))
      (setq meta (plist-put meta :preview (agent-sidebar--bounded-preview
                                          (plist-get meta :preview)))
            meta (plist-put meta :provider (plist-get entry :provider))
            meta (plist-put meta :parser (plist-get provider :parse)))
      meta)))

(defun agent-sidebar--ensure-parsed (entry)
  "Revalidate ENTRY's file before returning metadata for activation."
  (let* ((owner (current-buffer))
         (file (plist-get entry :id))
         (entries agent-sidebar--entries)
         (generation agent-sidebar--parse-generation)
         (cache agent-sidebar--parse-cache)
         (provider (agent-sidebar--entry-provider entry)))
    (unless (and entry (eq entry (gethash file entries)))
      (user-error "Transcript changed; select its current row"))
    (let ((meta (agent-sidebar--ensure-header file)))
      ;; The core rejects obsolete cache writes, but still returns the read.
      ;; An action must also reject that read before starting a session.
      (unless (and (buffer-live-p owner) (eq (current-buffer) owner)
                   (derived-mode-p 'agent-sidebar-mode)
                   (eq generation agent-sidebar--parse-generation)
                   (eq entries agent-sidebar--entries)
                   (eq entry (gethash file entries))
                   (eq provider (agent-sidebar--entry-provider entry))
                   (eq cache agent-sidebar--parse-cache)
                   (eq meta (plist-get (gethash file cache) :header)))
        (user-error "Sidebar changed while reading; select the transcript again"))
      (when (plist-get meta :error) (user-error "%s" (plist-get meta :error)))
      meta)))

(defun agent-sidebar--agent-shell-parse (entry)
  "Read ENTRY using the reviewed bounded Markdown parser."
  (let ((meta (agent-sidebar--parse-header-from-file (plist-get entry :id))))
    (plist-put meta :timestamp (plist-get meta :started))))

(defun agent-sidebar--read-jsonl (file limit)
  "Read complete JSON objects from at most LIMIT bytes of regular FILE.
Malformed records are skipped.  File and read-hook errors propagate."
  (let ((before (file-attributes file)))
    (unless (agent-sidebar--regular-file-attributes-p before)
      (user-error "Not a regular transcript file: %s" file))
    (with-temp-buffer
      (insert-file-contents file nil 0 limit)
      (let ((after (file-attributes file)) objects)
        (unless (agent-sidebar--regular-file-attributes-p after)
          (user-error "Transcript changed file type while reading: %s" file))
        (when (or (> (file-attribute-size before) limit) (> (file-attribute-size after) limit))
          (goto-char (point-max))
          (unless (bolp) (delete-region (line-beginning-position) (point-max))))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((object (condition-case nil
                            (json-parse-string
                             (buffer-substring-no-properties (point) (line-end-position))
                             :object-type 'alist :array-type 'list :null-object nil :false-object nil)
                          (json-error nil))))
            (when (and (consp object) (consp (car object)))
              (push object objects)))
          (forward-line 1))
        (nreverse objects)))))

(defun agent-sidebar--content-text (content)
  "Return the first nonempty text from CONTENT, skipping tool-result blocks."
  (cond ((stringp content) content)
        ((listp content)
         (seq-some (lambda (block)
                     (when (and (proper-list-p block) (equal (alist-get 'type block) "text")
                                (stringp (alist-get 'text block))
                                (not (string-empty-p (string-trim (alist-get 'text block)))))
                       (alist-get 'text block))) content))))

(defun agent-sidebar--claude-parse (entry)
  "Read up to 65536 bytes of Claude JSONL metadata for ENTRY."
  (condition-case err
      (let ((session-id (or (plist-get (plist-get entry :extras) :session-id)
                            (file-name-base (plist-get entry :id))))
            cwd model timestamp preview)
        (dolist (object (agent-sidebar--read-jsonl (plist-get entry :id) 65536))
          (let* ((value (alist-get 'message object))
                 (message (and (listp value) value))
                 (text (agent-sidebar--content-text (alist-get 'content message))))
            (unless timestamp (when (stringp (alist-get 'timestamp object))
                                (setq timestamp (alist-get 'timestamp object))))
            (unless cwd (when (stringp (alist-get 'cwd object)) (setq cwd (alist-get 'cwd object))))
            (unless model (when (stringp (alist-get 'model message)) (setq model (alist-get 'model message))))
            (when (and (null preview) (equal (alist-get 'type object) "user")
                       (not (alist-get 'isMeta object)) (stringp text))
              (let ((trimmed (string-trim text)))
                (unless (or (string-empty-p trimmed)
                            (string-match-p "\\`<\\(?:local-command-\\|command-\\)" trimmed))
                  (setq preview (car (split-string trimmed "\n" t))))))))
        (list :agent "Claude" :model model :timestamp timestamp :started timestamp
              :cwd cwd :session-id session-id :preview preview))
    (file-error (list :error (error-message-string err)))
    (user-error (list :error (error-message-string err)))))

(defun agent-sidebar--iso-to-seconds (timestamp)
  "Return seconds for TIMESTAMP, or nil if it is invalid."
  (when (stringp timestamp)
    (condition-case nil (float-time (date-to-time timestamp)) (error nil))))

(defun agent-sidebar--claude-sessions-for-cwd (cwd)
  "Return (ID . TIME) pairs whose JSONL records confirm CWD.
Directory membership and each file signature invalidate the cache."
  (unless agent-sidebar--claude-sessions-by-cwd-cache
    (setq agent-sidebar--claude-sessions-by-cwd-cache (make-hash-table :test 'equal)))
  (let* ((directory (expand-file-name (agent-sidebar--claude-encode-path cwd)
                                      agent-sidebar-claude-cli-projects-dir))
         (files (agent-sidebar--claude-files directory))
         (signature (mapcar (lambda (pair) (cons (car pair) (agent-sidebar--signature (cdr pair)))) files))
         (key (cons directory (file-name-as-directory (expand-file-name cwd))))
         (cached (gethash key agent-sidebar--claude-sessions-by-cwd-cache)))
    (if (equal (car cached) signature) (cdr cached)
      (let (sessions)
        (dolist (pair files)
          (let* ((file (car pair))
                 (entry (list :provider 'claude-cli :id file :extras (list :session-id (file-name-base file))))
                 (meta (or (agent-sidebar--cached-header file (cdr pair))
                           (agent-sidebar--claude-parse entry)))
                 (recorded (plist-get meta :cwd))
                 (seconds (agent-sidebar--iso-to-seconds (plist-get meta :timestamp))))
            (when (and seconds (stringp recorded)
                       (equal (cdr key) (file-name-as-directory (expand-file-name recorded))))
              (push (cons (plist-get meta :session-id) seconds) sessions))))
        (puthash key (cons signature sessions) agent-sidebar--claude-sessions-by-cwd-cache)
        sessions))))

(defun agent-sidebar--find-claude-session-for-transcript (agent cwd timestamp)
  "Recover one unambiguous Claude session within 60 seconds of TIMESTAMP."
  (when (and (stringp agent) (string-match-p "\\`[Cc]laude" agent)
             (stringp cwd) (not (string-empty-p cwd)))
    (when-let* ((seconds (agent-sidebar--iso-to-seconds timestamp)))
      (let ((matches (seq-filter (lambda (session) (< (abs (- seconds (cdr session))) 60))
                                 (agent-sidebar--claude-sessions-for-cwd cwd))))
        (when (= (length matches) 1) (caar matches))))))

(defun agent-sidebar--agent-shell-visit (entry)
  "Resume ENTRY through agent-shell, using its picker if the session id is
unknown."
  (let* ((meta (copy-sequence (agent-sidebar--ensure-parsed entry)))
         (file (plist-get entry :id))
         (cwd (agent-sidebar--working-directory file meta))
         (session (or (plist-get meta :session-id)
                      (agent-sidebar--find-claude-session-for-transcript
                       (plist-get meta :agent) cwd (plist-get meta :timestamp)))))
    (setq meta (plist-put meta :session-id session))
    (agent-sidebar--visit-header file meta (unless session 'prompt))))

(defun agent-sidebar--terminal-live-p (buffer)
  "Return non-nil if BUFFER owns a running terminal process."
  (and (buffer-live-p buffer)
       (let ((process (get-buffer-process buffer))) (and process (process-live-p process)))))

(defun agent-sidebar--launch-terminal (name program args directory)
  "Run PROGRAM and ARGS at DIRECTORY and return the terminal buffer.
Custom runners receive (NAME PROGRAM ARGS DIRECTORY) and return a buffer."
  (unless (and (stringp directory) (not (file-remote-p directory)) (file-directory-p directory))
    (user-error "Terminal directory must exist locally: %s" directory))
  (let* ((default-directory (file-name-as-directory directory))
         (executable (or (executable-find program) (user-error "Executable not found: %s" program)))
         (previous-buffers (buffer-list))
         (buffer
          (cond
           (agent-sidebar-terminal-function
            (funcall agent-sidebar-terminal-function name executable args default-directory))
           ((fboundp 'ghostel-exec)
            (let ((target (generate-new-buffer name)))
              (condition-case err
                  (progn (with-current-buffer target
                           (setq default-directory directory)
                           (ghostel-exec target executable args)) target)
                (error (when (buffer-live-p target) (kill-buffer target)) (signal (car err) (cdr err))))))
           ((fboundp 'vterm)
            (let ((vterm-shell (mapconcat #'shell-quote-argument (cons executable args) " "))
                  (window (seq-find
                           (lambda (candidate)
                             (and (not (window-dedicated-p candidate))
                                  (not (window-parameter candidate 'window-side))))
                           (window-list nil 'no-minibuffer))))
              (unless window (user-error "No ordinary window available for vterm"))
              ;; vterm uses pop-to-buffer-same-window, which rejects side windows.
              (save-window-excursion
                (with-selected-window window
                  (let ((default-directory (file-name-as-directory directory)))
                    (funcall #'vterm name))))))
           (t
            ;; `make-term' reuses a live process when its buffer name exists.
            ;; Session reuse is decided by the caller, never by this label.
            (let ((term-name name) (suffix 1))
              (while (get-buffer (concat "*" term-name "*"))
                (setq term-name (format "%s<%d>" name suffix)
                      suffix (1+ suffix)))
              (let ((target (get-buffer-create (concat "*" term-name "*"))))
                (condition-case err
                    (progn
                      (apply #'make-term term-name executable nil args)
                      (with-current-buffer target (term-char-mode))
                      target)
                  ((error quit)
                   (when (buffer-live-p target)
                     (with-current-buffer target
                       (when-let* ((process (get-buffer-process target)))
                         (delete-process process))
                       (let ((kill-buffer-query-functions nil))
                         (kill-buffer target))))
                   (signal (car err) (cdr err))))))))))
    (when (processp buffer) (setq buffer (process-buffer buffer)))
    (unless (buffer-live-p buffer) (user-error "Terminal runner did not return a live buffer"))
    (condition-case err
        (agent-sidebar--pop-to buffer)
      ((error quit)
       ;; A custom runner may return a buffer that it already owns.
       (unless (memq buffer previous-buffers)
         (condition-case nil
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when-let* ((process (get-buffer-process buffer)))
                   (delete-process process))
                 (let ((kill-buffer-query-functions nil))
                   (kill-buffer buffer))))
           ((error quit) nil)))
       (signal (car err) (cdr err))))
    buffer))

(defun agent-sidebar--visit-terminal (entry name program args &optional scope)
  "Resume ENTRY with PROGRAM, appending its session id to ARGS.
NAME labels the terminal.  SCOPE further distinguishes storage locations."
  (let* ((meta (agent-sidebar--ensure-parsed entry))
         (session (plist-get meta :session-id))
         (cwd (or (plist-get meta :cwd) (plist-get entry :repo))))
    (unless (and (stringp session) (not (string-empty-p session))) (user-error "Transcript has no session id"))
    (unless (and (stringp cwd) (file-name-absolute-p cwd)
                 (not (file-remote-p cwd)) (file-directory-p cwd))
      (user-error "Transcript has no existing local working directory"))
    (let* ((identity (append (list (plist-get entry :provider) session
                                  (file-name-as-directory (expand-file-name cwd)))
                            (when scope (list scope))))
           (live (seq-find (lambda (buffer)
                            (and (agent-sidebar--terminal-live-p buffer)
                                 (equal identity (buffer-local-value 'agent-sidebar--terminal-session buffer))))
                          (buffer-list))))
      (if live (agent-sidebar--pop-to live)
        (let ((buffer (agent-sidebar--launch-terminal
                       (format "*%s-resume: %s*" name (substring session 0 (min 8 (length session))))
                       program (append args (list session)) cwd)))
          (with-current-buffer buffer
            (setq-local agent-sidebar--terminal-session identity)
            (setq-local agent-sidebar--terminal-file (plist-get entry :id))))))))

(defun agent-sidebar--claude-visit (entry)
  "Resume ENTRY in a Claude CLI terminal, reusing its running session."
  (agent-sidebar--visit-terminal entry "claude" agent-sidebar-claude-cli-command
                                 agent-sidebar-claude-cli-resume-args))

(defun agent-sidebar--codex-list ()
  "Discover regular Codex rollouts without following directory symlinks."
  (let ((base (expand-file-name "sessions/" agent-sidebar-codex-home)) entries)
    (unless (file-remote-p base)
      (let ((pending (list (cons base 0))))
        (while pending
          (pcase-let ((`(,directory . ,depth) (pop pending)))
            (unless (file-symlink-p directory)
              (dolist (pair (condition-case nil
                               (directory-files-and-attributes directory t "\\`[^.]" t)
                             (file-error nil)))
                (let* ((file (car pair)) (attrs (cdr pair))
                       (name (file-name-nondirectory file)))
                  (cond
                   ((and (< depth 3) (eq t (file-attribute-type attrs))
                         (string-match-p (if (= depth 0) "\\`[0-9]\\{4\\}\\'" "\\`[0-9]\\{2\\}\\'") name))
                    (push (cons file (1+ depth)) pending))
                   ((and (agent-sidebar--regular-file-attributes-p attrs)
                         (string-match-p "\\`rollout-.*\\.jsonl\\'" name))
                    (push (list :provider 'codex-cli :id file :agent "Codex" :attrs attrs
                                :extras (list :codex-home (expand-file-name agent-sidebar-codex-home)))
                          entries))))))))))
    entries))

(defun agent-sidebar--codex-user-text (content)
  "Extract a user prompt from CONTENT, excluding leading injected context.
Retain user text after a complete context wrapper and markup inside a prompt."
  (seq-some
   (lambda (block)
     (let ((text (and (proper-list-p block) (equal (alist-get 'type block) "input_text")
                      (alist-get 'text block))))
       (when (stringp text)
         (setq text (string-trim text))
         (let ((start 0)
               (context-tag
                "<\\(environment_context\\|INSTRUCTIONS\\|permissions\\|skills_instructions\\|user_instructions\\|recommended_plugins\\)\\(?:[[:space:]][^>]*\\)?>"))
           ;; Consume leading wrappers without copying their remaining suffix
           ;; on every iteration.  An incomplete context block has no prompt.
           (while (and (< start (length text))
                       (string-match context-tag text start)
                       (= (match-beginning 0) start))
             (let ((closing (concat "</" (match-string 1 text) ">"))
                   (body-start (match-end 0)))
               (setq start
                     (if (string-match (regexp-quote closing) text body-start)
                         (or (string-match "[^ \t\r\n]" text (match-end 0))
                             (length text))
                       (length text)))))
           (when (> start 0) (setq text (substring text start))))
         (unless (or (string-empty-p text)
                     (string-match-p "\\`# AGENTS\\.md instructions" text))
           text))))
   (and (listp content) content)))

(defun agent-sidebar--codex-parse (entry)
  "Read at most 262144 bytes of native Codex rollout metadata for ENTRY.
Use the thread id in session_meta, never a prompt's claimed session id.
Choose the first user prompt in file order across both native record forms."
  (condition-case err
      (let (session cwd timestamp model preview)
        (dolist (object (agent-sidebar--read-jsonl (plist-get entry :id) 262144))
          (let ((payload (alist-get 'payload object)))
            (when (listp payload)
              (pcase (alist-get 'type object)
                ("session_meta"
                 ;; Forked rollouts can contain older metadata after their own.
                 (unless session
                   (let ((id (alist-get 'id payload)))
                     (when (and (stringp id) (not (string-empty-p id)))
                       (setq session id cwd (alist-get 'cwd payload)
                             timestamp (alist-get 'timestamp payload))))))
                ("turn_context"
                 (unless model
                   (when (stringp (alist-get 'model payload))
                     (setq model (alist-get 'model payload)))))
                ("event_msg"
                 (when (and (null preview) (equal (alist-get 'type payload) "user_message")
                            (stringp (alist-get 'message payload)))
                   (setq preview
                         (agent-sidebar--codex-user-text
                          (list (list (cons 'type "input_text")
                                      (cons 'text (alist-get 'message payload))))))))
                ("response_item"
                 (when (and (null preview) (equal (alist-get 'type payload) "message")
                            (equal (alist-get 'role payload) "user"))
                   (setq preview (agent-sidebar--codex-user-text (alist-get 'content payload)))))))))
        (unless (and (stringp session)
                     (string-match-p
                      "\\`[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}\\'"
                      session))
          (user-error "No valid Codex thread id in rollout prefix"))
        (list :agent "Codex" :session-id session :cwd (and (stringp cwd) cwd)
              :timestamp (and (stringp timestamp) timestamp) :model model
              :preview (and preview (car (split-string (string-trim preview) "\n" t)))))
    (file-error (list :error (error-message-string err)))
    (user-error (list :error (error-message-string err)))))

(defun agent-sidebar--codex-environment ()
  "Return a child environment using the sidebar's local Codex data directory."
  (when (file-remote-p agent-sidebar-codex-home) (user-error "Codex home must be local"))
  (cons (concat "CODEX_HOME=" (expand-file-name agent-sidebar-codex-home))
        (seq-remove (lambda (value) (string-prefix-p "CODEX_HOME=" value)) process-environment)))

(defun agent-sidebar--require-codex-terminal ()
  "Make ghostel available for Codex unless a custom terminal runner is set."
  (unless (or agent-sidebar-terminal-function (fboundp 'ghostel-exec)
              (and (require 'ghostel nil t) (fboundp 'ghostel-exec)))
    (user-error "Codex CLI requires ghostel or agent-sidebar-terminal-function")))

(defun agent-sidebar--codex-visit (entry)
  "Resume ENTRY as a Codex CLI session in ghostel."
  (agent-sidebar--require-codex-terminal)
  (let* ((agent-sidebar-codex-home (or (plist-get (plist-get entry :extras) :codex-home)
                                      agent-sidebar-codex-home))
         (process-environment (agent-sidebar--codex-environment)))
    (agent-sidebar--visit-terminal entry "codex" agent-sidebar-codex-cli-command '("resume")
                                   (agent-sidebar--codex-home-identity agent-sidebar-codex-home))))

(defun agent-sidebar--codex-home-identity (home)
  "Return the canonical local directory identity for Codex HOME."
  (unless (and (stringp home) (not (string-empty-p home))
               (not (file-remote-p home)))
    (user-error "Codex home must be a nonempty local directory"))
  (file-name-as-directory (file-truename (expand-file-name home))))

(defun agent-sidebar--codex-base-config (config)
  "Return CONFIG without a sidebar's per-session Codex wrapper."
  (or (map-elt config :agent-sidebar-codex-base-config) config))

(defun agent-sidebar--codex-session-config (config home)
  "Copy CONFIG, preserving HOME in every ACP client it creates.
HOME keeps the selected path spelling.  Other client environment and
authentication settings are supplied by the original client maker."
  (let* ((base (agent-sidebar--codex-base-config config))
         (maker (map-elt base :client-maker))
         (selected-home (expand-file-name home))
         (client-maker
          (lambda (buffer)
            (when-let* ((client (copy-alist (funcall maker buffer))))
              ;; ACP prepends this list before process-environment.  Keep
              ;; the home here so reload and deferred starts retain it.
              (setf (alist-get :environment-variables client)
                    (cons (concat "CODEX_HOME=" selected-home)
                          (seq-remove
                           (lambda (value)
                             (or (equal value "CODEX_HOME")
                                 (string-prefix-p "CODEX_HOME=" value)))
                           (map-elt client :environment-variables))))
              client)))
         (wrapped (copy-alist base)))
    (setf (alist-get :agent-sidebar-codex-base-config wrapped) base
          (alist-get :agent-sidebar-codex-home wrapped) selected-home
          (alist-get :client-maker wrapped) client-maker)
    wrapped))

(defun agent-sidebar--codex-buffer-home (buffer)
  "Return BUFFER's known Codex home identity, or nil when unknown.
Inspect the actual client environment before stored session metadata.
Do not infer an ordinary agent-shell buffer's home from ambient settings."
  (with-current-buffer buffer
    (let* ((state agent-shell--state)
           (config (map-elt state :agent-config))
           (explicit
            (seq-find (lambda (value)
                        (or (equal value "CODEX_HOME")
                            (string-prefix-p "CODEX_HOME=" value)))
                      (map-nested-elt state '(:client :environment-variables))))
           (home (if explicit
                     (and (string-prefix-p "CODEX_HOME=" explicit)
                          (substring explicit (length "CODEX_HOME=")))
                   (or (map-elt config :agent-sidebar-codex-home)
                       agent-sidebar--codex-session-home))))
      (when (and (stringp home) (not (string-empty-p home)))
        (condition-case nil
            (agent-sidebar--codex-home-identity home)
          (file-error nil)
          (user-error nil))))))

(defun agent-sidebar--codex-live-buffer (session directory config home)
  "Find SESSION in DIRECTORY with base CONFIG and canonical HOME."
  (when (and (stringp session) (not (string-empty-p session)))
    (seq-find
     (lambda (buffer)
       (with-current-buffer buffer
         (let* ((active (map-nested-elt agent-shell--state '(:session :id)))
                (current (if (and (stringp active) (not (string-empty-p active)))
                             active
                           (map-elt agent-shell--state :resume-session-id))))
           (and (equal session current)
                (equal (agent-sidebar--codex-base-config config)
                       (agent-sidebar--codex-base-config
                        (map-elt agent-shell--state :agent-config)))
                (equal (file-name-as-directory (expand-file-name directory))
                       (file-name-as-directory (expand-file-name default-directory)))
                (equal home (agent-sidebar--codex-buffer-home buffer))))))
     (agent-shell-buffers))))

(defun agent-sidebar--codex-start-agent-shell (directory &optional session)
  "Start or reuse Codex through agent-shell in DIRECTORY for SESSION."
  (let* ((config (agent-sidebar--select-config "Codex"))
         (home (agent-sidebar--codex-home-identity agent-sidebar-codex-home))
         (existing (agent-sidebar--codex-live-buffer session directory config home))
         (process-environment (agent-sidebar--codex-environment)))
    (or existing
        (let ((buffer (agent-sidebar--start
                       directory
                       (agent-sidebar--codex-session-config config agent-sidebar-codex-home)
                       session)))
          (with-current-buffer buffer (setq-local agent-sidebar--codex-session-home home))
          buffer))))

(defun agent-sidebar--codex-agent-shell-visit (entry)
  "Resume the native Codex rollout ENTRY through agent-shell."
  (let* ((agent-sidebar-codex-home (or (plist-get (plist-get entry :extras) :codex-home)
                                      agent-sidebar-codex-home))
         (meta (agent-sidebar--ensure-parsed entry))
         (directory (plist-get meta :cwd))
         (session (plist-get meta :session-id)))
    (unless (and (stringp directory) (file-name-absolute-p directory)
                 (not (file-remote-p directory)) (file-directory-p directory))
      (user-error "Codex rollout has no existing local working directory"))
    (unless (and (stringp session) (not (string-empty-p session)))
      (user-error "Codex rollout has no session id"))
    (let ((buffer (agent-sidebar--codex-start-agent-shell (file-name-as-directory directory) session)))
      (with-current-buffer buffer
        (setq-local agent-sidebar--agent-shell-file (plist-get entry :id)))
      (agent-sidebar--pop-to buffer))))

(defun agent-sidebar--codex-buffer-session (buffer)
  "Return BUFFER's current Codex session id and home, or nil.
An established session takes precedence over a pending resume request."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'agent-shell-mode)
                 (eq 'codex
                     (map-elt (agent-sidebar--codex-base-config
                               (map-elt agent-shell--state :agent-config))
                              :identifier)))
        (let* ((active (map-nested-elt agent-shell--state '(:session :id)))
               (session (if (and (stringp active) (not (string-empty-p active)))
                            active
                          (map-elt agent-shell--state :resume-session-id)))
               (home (agent-sidebar--codex-buffer-home buffer)))
          (when (and (stringp session) (not (string-empty-p session)) home)
            (cons session home)))))))

(defun agent-sidebar--codex-owned-file-p (file)
  "Return non-nil when an open Codex ACP session owns native rollout FILE.
Read only FILE's bounded metadata prefix, after locating a matching home.
Do not depend on a path marker that new or reloaded sessions do not have."
  (when (and (stringp file) (not (file-remote-p file)) (file-exists-p file))
    (let ((canonical (file-truename file)) homes)
      (dolist (buffer (buffer-list))
        (when-let* ((identity (agent-sidebar--codex-buffer-session buffer))
                    (relative (file-relative-name canonical
                                                  (expand-file-name "sessions/" (cdr identity)))))
          (when (string-match-p
                 "\\`\\(?:[0-9]\\{4\\}/\\(?:[0-9]\\{2\\}/\\(?:[0-9]\\{2\\}/\\)?\\)?\\)?rollout-[^/]*\\.jsonl\\'"
                 relative)
            (cl-pushnew (cdr identity) homes :test #'equal))))
      (when homes
        ;; Re-read the selected file rather than trusting the rendered cache.
        (let* ((meta (agent-sidebar--codex-parse (list :id file)))
               (session (plist-get meta :session-id)))
          (when (plist-get meta :error)
            (user-error "Cannot verify open Codex session ownership: %s"
                        (plist-get meta :error)))
          (seq-some
           (lambda (buffer)
             ;; A read hook can reload a session into a replacement buffer.
             ;; Check all current owners within the verified storage scope.
             (when-let* ((identity (agent-sidebar--codex-buffer-session buffer)))
               (and (equal session (car identity))
                    (member (cdr identity) homes))))
           (buffer-list)))))))

(defun agent-sidebar--owned-file-p (file)
  "Return non-nil if a terminal or agent-shell owns FILE, including aliases."
  (or (seq-some (lambda (buffer)
                  (let ((owned (or (buffer-local-value 'agent-sidebar--terminal-file buffer)
                                   (buffer-local-value 'agent-sidebar--agent-shell-file buffer))))
                    (and (stringp owned)
                         (or (agent-sidebar--terminal-live-p buffer)
                             (with-current-buffer buffer (derived-mode-p 'agent-shell-mode)))
                         (or (equal owned file) (file-equal-p owned file))))) (buffer-list))
      (agent-sidebar--codex-owned-file-p file)))

(agent-sidebar-register-provider
 (list :id 'agent-shell :name "agent-shell" :list #'agent-sidebar--agent-shell-list
       :parse #'agent-sidebar--agent-shell-parse :visit #'agent-sidebar--agent-shell-visit))

(agent-sidebar-register-provider
 (list :id 'claude-cli :name "Claude CLI" :list #'agent-sidebar--claude-list
       :parse #'agent-sidebar--claude-parse :visit #'agent-sidebar--claude-visit))

(agent-sidebar-register-provider
 (list :id 'codex-cli :name "Codex CLI" :list #'agent-sidebar--codex-list
       :parse #'agent-sidebar--codex-parse :visit #'agent-sidebar--codex-visit))

(defun agent-sidebar--entry-field (entry key)
  "Return KEY from cached or frozen metadata, then ENTRY."
  (or (plist-get (agent-sidebar--cached-meta entry) key) (plist-get entry key)))

(defun agent-sidebar--entry-agent (entry)
  "Return the agent label for ENTRY."
  (agent-sidebar--entry-field entry :agent))

(defun agent-sidebar--entry-preview (entry)
  "Return the first prompt for ENTRY."
  (agent-sidebar--entry-field entry :preview))

(defun agent-sidebar--entry-repo (entry)
  "Return a known project root or the recorded directory for ENTRY."
  (or (plist-get entry :repo)
      (let ((cwd (agent-sidebar--entry-field entry :cwd)))
        (when (and (stringp cwd) (file-name-absolute-p cwd))
          (file-name-as-directory (expand-file-name cwd))))))

(defun agent-sidebar--entry-timestamp (entry)
  "Return a short display date for ENTRY."
  (let ((timestamp (agent-sidebar--entry-field entry :timestamp)))
    (if (and (stringp timestamp)
             (string-match "\\`[0-9]\\{4\\}-\\([0-9]\\{2\\}-[0-9]\\{2\\}\\)[T ]\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)" timestamp))
        (concat (match-string 1 timestamp) " " (match-string 2 timestamp))
      (format-time-string "%m-%d %H:%M" (seconds-to-time (or (plist-get entry :mtime) 0))))))

(defun agent-sidebar--entry-date-key (entry)
  "Return the date used to group ENTRY."
  (let ((timestamp (agent-sidebar--entry-field entry :timestamp)))
    (if (and (stringp timestamp)
             (string-match "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" timestamp))
        (match-string 0 timestamp)
      (format-time-string "%Y-%m-%d" (seconds-to-time (or (plist-get entry :mtime) 0))))))

(defun agent-sidebar--group-key (entry level)
  "Return ENTRY's key at grouping LEVEL."
  (pcase level
    ('package (plist-get entry :provider))
    ('repo (agent-sidebar--entry-repo entry))
    ('model (or (agent-sidebar--entry-field entry :model) (agent-sidebar--entry-agent entry) "?"))
    ('date (agent-sidebar--entry-date-key entry))))

(defun agent-sidebar--validate-grouping (levels)
  "Reject unsupported or repeated grouping LEVELS before starting a view."
  (unless (and (proper-list-p levels) (<= (length levels) 4)
               (= (length levels) (length (delete-dups (copy-sequence levels))))
               (cl-every (lambda (level) (memq level '(package repo model date))) levels))
    (user-error "Grouping must contain package, repo, model, or date without repeats")))

(defun agent-sidebar--matches-filter-p (entry)
  "Match the literal filter against ENTRY, retaining metadata that is still
pending."
  (let ((meta (agent-sidebar--cached-meta entry)))
    (or (string-empty-p agent-sidebar--filter) (null meta)
        (string-search
         (downcase agent-sidebar--filter)
         (downcase (mapconcat #'identity
                              (seq-filter #'stringp
                                          (list (plist-get entry :id)
                                                (agent-sidebar--entry-repo entry)
                                                (plist-get meta :preview) (plist-get meta :agent)
                                                (plist-get meta :model) (plist-get meta :session-id)
                                                (plist-get meta :cwd)
                                                (plist-get (agent-sidebar--entry-provider entry) :name)))
                              "\n"))))))

(defun agent-sidebar--node (level key breadcrumb)
  "Create a grouping node for LEVEL, KEY, and BREADCRUMB."
  (list :level level :key key :breadcrumb breadcrumb :count 0
        :children (cons nil nil) :index (make-hash-table :test 'equal)
        :rows (cons nil nil)))

(iter-defun agent-sidebar--plan-view (entries levels)
  "Build a grouped snapshot of ENTRIES and LEVELS, yielding after each entry.
Capture metadata once per entry so filtering, grouping, and rows agree."
  (let ((root (agent-sidebar--node nil nil nil)) (repos (make-hash-table :test 'equal)))
    (dolist (original entries)
      (let ((entry (plist-put (copy-sequence original) :meta (agent-sidebar--cached-meta original))))
	(when (agent-sidebar--matches-filter-p entry)
	  (when-let* ((repo (agent-sidebar--entry-repo entry))) (puthash repo t repos))
	  (let ((node root) breadcrumb)
	    (dolist (level levels)
	      (let* ((key (agent-sidebar--group-key entry level))
		     (index (plist-get node :index))
		     (child (gethash key index)))
		(setq breadcrumb (append breadcrumb (list (cons level key))))
		(unless child
		  (setq child (agent-sidebar--node level key breadcrumb))
		  (puthash key child index)
		  (agent-sidebar--append-item (plist-get node :children) child))
		(cl-incf (plist-get child :count))
		(setq node child)))
	    (agent-sidebar--append-item (plist-get node :rows) entry))))
      (iter-yield nil))
    (list root (agent-sidebar--disambiguated-names (hash-table-keys repos)))))

(defun agent-sidebar--group-name (node names)
  "Return NODE's display name using the snapshot's repository NAMES."
  (let ((key (plist-get node :key)))
    (pcase (plist-get node :level)
      ('package (or (plist-get (alist-get key agent-sidebar-providers) :name) (format "%s" key)))
      ('repo (if key (or (gethash key names) key) "Unknown project"))
      (_ (or key "?")))))

(defun agent-sidebar--render-group-header (node depth names collapsed)
  "Insert NODE's header at DEPTH using NAMES and COLLAPSED state."
  (let* ((level (plist-get node :level))
         (face (pcase level ('package 'agent-sidebar-package-face) ('repo 'agent-sidebar-project-face)
                      ('model 'agent-sidebar-agent-face) (_ 'agent-sidebar-date-face)))
         (start (point)))
    (insert (propertize (format "%s%s %s (%d)" (make-string (* 2 depth) ?\s)
                                (if collapsed "▸" "▾") (agent-sidebar--group-name node names)
                                (plist-get node :count)) 'face face))
    (add-text-properties start (point)
                         (list 'agent-sidebar-group (plist-get node :breadcrumb)
                               'agent-sidebar-collapsed collapsed))
    (insert "\n")))

(defun agent-sidebar--render-entry-row (entry depth mark)
  "Insert ENTRY at DEPTH with its frozen metadata and MARK."
  (let* ((meta (agent-sidebar--cached-meta entry)) (file (plist-get entry :id))
         (preview (plist-get meta :preview)) (start (point)))
    (insert (make-string (+ 2 (* 2 depth)) ?\s)
            (if (eq mark 'delete) (propertize "D" 'face 'agent-sidebar-mark-face) " ") " "
            (propertize (agent-sidebar--entry-timestamp entry)
                        'face (if meta 'agent-sidebar-date-face 'agent-sidebar-placeholder-face))
            (cond ((plist-get meta :error) " [unreadable transcript]")
                  (preview (concat ": " preview))
                  (meta (concat ": " (file-name-base file)))
                  (t ": …")))
    (add-text-properties
     start (point)
     (list 'agent-sidebar-entry entry 'agent-sidebar-file file
           'help-echo (format "%s\nProvider: %s\nAgent: %s\nModel: %s\nSession: %s\nCwd: %s%s"
                              file (plist-get entry :provider) (or (plist-get meta :agent) "?")
                              (or (plist-get meta :model) "?") (or (plist-get meta :session-id) "?")
                              (or (plist-get meta :cwd) (agent-sidebar--entry-repo entry) "?")
                              (if (plist-get meta :error) (concat "\n" (plist-get meta :error)) ""))))
    (insert "\n")))

(iter-defun agent-sidebar--render-node (node output depth names marks folds)
  "Render NODE into OUTPUT, yielding at each row and header."
  (let ((folded (gethash (plist-get node :breadcrumb) folds)))
    (when (plist-get node :level)
      (with-current-buffer output (agent-sidebar--render-group-header node depth names folded))
      (setq depth (1+ depth))
      (iter-yield nil))
    (unless folded
      (dolist (entry (car (plist-get node :rows)))
	(with-current-buffer output
	  (agent-sidebar--render-entry-row entry depth (gethash (plist-get entry :id) marks)))
	(iter-yield nil))
      (dolist (child (car (plist-get node :children)))
	(iter-yield-from (agent-sidebar--render-node child output depth names marks folds))))))

(iter-defun agent-sidebar--render-snapshot (output _groups _names)
  "Construct a provider view in OUTPUT using the shared idle scheduler."
  (let* ((levels (copy-sequence agent-sidebar-grouping))
	 (entries agent-sidebar--entry-list)
	 (marks agent-sidebar--marks) (folds agent-sidebar--collapsed))
    (agent-sidebar--validate-grouping levels)
    (let ((plan (iter-yield-from (agent-sidebar--plan-view entries levels))))
      (iter-yield-from (agent-sidebar--render-node (car plan) output 0 (cadr plan) marks folds)))
    (with-current-buffer output
      (when (= (buffer-size) 0)
	(insert (if entries "  (no matching chats)\n" "  (no chats found)\n"))))))

(defun agent-sidebar--render-contents ()
  "Insert a complete view for an explicit redraw, without rediscovery."
  (let ((iterator (agent-sidebar--render-snapshot (current-buffer) nil nil)))
    (condition-case nil (while t (iter-next iterator)) (iter-end-of-sequence nil))))

(defun agent-sidebar-toggle-group ()
  "Toggle the group header on the current line."
  (interactive)
  (let ((crumb (get-text-property (line-beginning-position) 'agent-sidebar-group)))
    (unless crumb (user-error "No group header at point"))
    (if (gethash crumb agent-sidebar--collapsed) (remhash crumb agent-sidebar--collapsed)
      (puthash crumb t agent-sidebar--collapsed))
    (agent-sidebar--redraw)))

(defun agent-sidebar-set-grouping (levels)
  "Group entries by ordered LEVELS.  An empty list gives a flat view."
  (interactive (list (mapcar #'intern
                             (completing-read-multiple "Group by (ordered, empty for flat): "
                                                       '("package" "repo" "model" "date") nil t))))
  (agent-sidebar--validate-grouping levels)
  (setq-local agent-sidebar-grouping (copy-sequence levels))
  (agent-sidebar--redraw))

(defun agent-sidebar--current-entry ()
  "Return the current entry for the displayed row, or report a stale row."
  (let* ((displayed (get-text-property (line-beginning-position) 'agent-sidebar-entry))
         (entry (and displayed (gethash (plist-get displayed :id) agent-sidebar--entries))))
    (unless entry (user-error "No current transcript at point"))
    entry))

(defun agent-sidebar-visit ()
  "Visit the provider entry on the current line, or fold a group."
  (interactive)
  (if (get-text-property (line-beginning-position) 'agent-sidebar-group)
      (agent-sidebar-toggle-group)
    (let ((entry (agent-sidebar--current-entry)))
      (funcall (plist-get (agent-sidebar--entry-provider entry) :visit) entry))))

(defun agent-sidebar-mouse-visit (event)
  "Select EVENT's row and visit it."
  (interactive "e")
  (mouse-set-point event)
  (agent-sidebar-visit))

(defun agent-sidebar-visit-in-agent-shell ()
  "Resume the selected Codex rollout or agent-shell transcript in agent-shell."
  (interactive)
  (let ((entry (agent-sidebar--current-entry)))
    (pcase (plist-get entry :provider)
      ('codex-cli (agent-sidebar--codex-agent-shell-visit entry))
      ('agent-shell (agent-sidebar--agent-shell-visit entry))
      (_ (user-error "Select a Codex or agent-shell transcript")))))

(defun agent-sidebar--new-context-for-entry (entry)
  "Return the provider, agent, directory, and Codex home for current ENTRY."
  (let* ((meta (agent-sidebar--ensure-parsed entry))
         (provider (plist-get entry :provider))
         (directory (if (eq provider 'agent-shell)
                        (agent-sidebar--working-directory (plist-get entry :id) meta)
                      (or (plist-get meta :cwd)
                          (and (not (eq provider 'codex-cli)) (plist-get entry :repo))))))
    (unless (and (stringp directory) (file-name-absolute-p directory)
                 (or (eq provider 'agent-shell) (not (file-remote-p directory)))
                 (file-directory-p directory))
      (user-error "Selected transcript has no existing working directory"))
    (list :provider provider
          :agent (or (plist-get meta :agent) (plist-get entry :agent))
          :directory (file-name-as-directory (expand-file-name directory))
          :codex-home (when (eq provider 'codex-cli)
                        (file-name-as-directory
                         (expand-file-name
                          (or (plist-get (plist-get entry :extras) :codex-home)
                              agent-sidebar-codex-home)))))))

(defun agent-sidebar--new-context-for-group (breadcrumb)
  "Return one session context for BREADCRUMB, or request a specific row.
Grouping and filtering determine membership.  They do not select a backend."
  (let* ((filter agent-sidebar--filter)
         (member-p (lambda (entry)
                     (let ((agent-sidebar--filter filter))
                       (and (agent-sidebar--matches-filter-p entry)
                            (cl-every (lambda (part)
                                        (equal (cdr part) (agent-sidebar--group-key entry (car part))))
                                      breadcrumb)))))
         (entries (seq-filter member-p agent-sidebar--entry-list))
         contexts)
    (unless entries (user-error "Group has no current entries; select a transcript row"))
    (dolist (entry entries)
      (let ((context (agent-sidebar--new-context-for-entry entry)))
        (unless (and (equal filter agent-sidebar--filter) (funcall member-p entry))
          (user-error "Group changed while reading; refresh or select a transcript row"))
        (cl-pushnew context contexts :test #'equal)
        (when (cdr contexts)
          (user-error "Group has multiple session contexts; select a transcript row"))))
    (car contexts)))

(defun agent-sidebar-new-session (&optional use-agent-shell)
  "Start a new session in the entry or repository at point.
USE-AGENT-SHELL (a prefix argument) starts Codex through agent-shell.
A group must identify one provider, agent, working directory, and Codex home."
  (interactive "P")
  (let* ((crumb (get-text-property (line-beginning-position) 'agent-sidebar-group))
         (context (cond
                   ((get-text-property (line-beginning-position) 'agent-sidebar-entry)
                    (agent-sidebar--new-context-for-entry (agent-sidebar--current-entry)))
                   (crumb (agent-sidebar--new-context-for-group crumb))
                   (t (list :provider 'agent-shell :directory (car agent-sidebar--context-roots)))))
         (provider (plist-get context :provider))
         (agent-sidebar-codex-home (or (plist-get context :codex-home) agent-sidebar-codex-home))
         (directory (plist-get context :directory)))
    (unless (and directory (file-directory-p directory)) (user-error "No existing project at point"))
    (pcase provider
      ('agent-shell
       (agent-sidebar--pop-to (agent-sidebar--start
                               directory (agent-sidebar--select-config
                                          (plist-get context :agent)))))
      ('claude-cli (agent-sidebar--launch-terminal "*claude-new*" agent-sidebar-claude-cli-command nil directory))
      ('codex-cli
       (if use-agent-shell
           (agent-sidebar--pop-to (agent-sidebar--codex-start-agent-shell directory))
         (agent-sidebar--require-codex-terminal)
         (let ((process-environment (agent-sidebar--codex-environment)))
           (agent-sidebar--launch-terminal "*codex-new*" agent-sidebar-codex-cli-command nil directory))))
      (_ (user-error "Provider does not support new sessions: %s" provider)))))

(defun agent-sidebar-toggle-sidebar ()
  "Toggle the provider sidebar in the selected frame."
  (interactive)
  (if (agent-sidebar-showing-sidebar-p) (agent-sidebar-hide-sidebar)
    (agent-sidebar-show-sidebar)
    (when agent-sidebar-pop-to-sidebar-on-toggle-open
      (when-let* ((window (agent-sidebar--sidebar-window))) (select-window window)))))

;;; Mode and keymap

(defvar agent-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agent-sidebar-visit)
    (define-key map (kbd "o") #'agent-sidebar-open-transcript)
    (define-key map (kbd "/") #'agent-sidebar-set-filter)
    (define-key map (kbd "N") #'agent-sidebar-new-session)
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
    (define-key map (kbd "G") #'agent-sidebar-set-grouping)
    (define-key map (kbd "A") #'agent-sidebar-visit-in-agent-shell)
    map)
  "Keymap for `agent-sidebar-mode'.")

(define-derived-mode agent-sidebar-mode special-mode "AgentChats"
  "Browse conversations across providers.
RET resumes, o reads, / filters, G changes grouping, and N starts a session.
A resumes a Codex rollout through agent-shell; C-u N starts a Codex agent-shell.
The d key marks a transcript, u removes its mark, and x confirms deletion.
\\{agent-sidebar-mode-map}"
  :group 'agent-sidebar
  (setq-local truncate-lines t)
  (setq-local window-size-fixed agent-sidebar-window-fixed)
  (setq-local cursor-in-non-selected-windows nil)
  (setq-local agent-sidebar--parse-cache (make-hash-table :test 'equal))
  (setq-local agent-sidebar--marks (make-hash-table :test 'equal))
  (setq-local agent-sidebar--collapsed (make-hash-table :test 'equal))
  (cl-pushnew (current-buffer) agent-sidebar--timer-buffers)
  (add-hook 'post-command-hook #'agent-sidebar--reset-idle-timers)
  (add-hook 'kill-buffer-hook #'agent-sidebar--cleanup nil t)
  (add-hook 'change-major-mode-hook #'agent-sidebar--cleanup nil t)
  (setq-local agent-sidebar--entries (make-hash-table :test 'equal))
  (setq-local agent-sidebar--entry-list nil)
  (setq-local agent-sidebar--claude-sessions-by-cwd-cache (make-hash-table :test 'equal))
  (setq-local agent-sidebar--collect-function #'agent-sidebar--collect)
  (setq-local agent-sidebar--parse-file-function #'agent-sidebar--parse-file)
  (setq-local agent-sidebar--render-contents-function #'agent-sidebar--render-contents)
  (setq-local agent-sidebar--render-snapshot-function #'agent-sidebar--render-snapshot)
  (setq-local agent-sidebar--owned-file-p-function #'agent-sidebar--owned-file-p)
  (setq-local agent-sidebar--row-identity-properties '(agent-sidebar-file agent-sidebar-group))
  (agent-sidebar--schedule-refresh))

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal agent-sidebar-mode-map
      (kbd "RET") #'agent-sidebar-visit
      (kbd "o")   #'agent-sidebar-open-transcript
      (kbd "/")   #'agent-sidebar-set-filter
      (kbd "N")   #'agent-sidebar-new-session
      (kbd "A")   #'agent-sidebar-visit-in-agent-shell
      (kbd "gr")  #'agent-sidebar-refresh
      (kbd "gg")  #'evil-goto-first-line
      (kbd "G")   #'agent-sidebar-set-grouping
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
