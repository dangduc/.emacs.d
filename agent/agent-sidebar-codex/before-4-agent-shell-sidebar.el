;;; agent-shell-sidebar.el --- Sidebar browser for agent-shell chats -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Keywords: agent-shell, tools
;; Package-Requires: ((emacs "29.1") (agent-shell "0.74.3") (project "0.9"))

;;; Commentary:
;;
;; Persistent sidebar listing agent-shell transcripts across all known
;; projects.  Transcripts live at <project>/.agent-shell/transcripts/*.md.
;;
;; RET on a row starts (or reuses) an agent-shell that resumes the
;; transcript's session:
;;   1. If a live agent-shell buffer already exists for that session-id,
;;      switch to it.
;;   2. Else start a new shell via `agent-shell--start' with
;;      `default-directory' bound to the transcript's recorded working
;;      directory, auto-matching the agent config by name.  When the
;;      transcript has a Session ID, it is resumed; otherwise a fresh
;;      shell starts in the same directory with the same agent.
;;
;; Header metadata (agent, timestamp, first user prompt) is parsed lazily
;; via a chunked idle-timer so hundreds of transcripts don't block; results
;; are cached by path, modification time, size, inode, and schema so parser
;; changes automatically invalidate old entries.  Projects with clashing
;; basenames are disambiguated by prepending parent path components until
;; unique.
;;
;; Entry point: M-x agent-shell-sidebar-toggle-sidebar.
;; RET resumes, o reads the transcript, TAB folds, / filters metadata,
;; N starts a new session in the project, g refreshes, and q hides.
;; C-u / clears the filter.  d/u mark or unmark and x confirms deletion.
;; Known remote projects are skipped unless explicitly enabled.

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

;;; Customization

(defgroup agent-shell-sidebar nil
  "Sidebar browser for agent-shell chat transcripts."
  :group 'agent-shell)

(defcustom agent-shell-sidebar-name "*:AgentChats:*"
  "Name of the sidebar buffer."
  :type 'string)

(defcustom agent-shell-sidebar-width 55
  "Width of the sidebar window."
  :type 'integer)

(defcustom agent-shell-sidebar-display-alist '((side . left) (slot . 2))
  "Alist passed to `display-buffer-in-side-window'."
  :type 'alist)

(defcustom agent-shell-sidebar-pop-to-sidebar-on-toggle-open t
  "Whether to select the sidebar window after toggling it open."
  :type 'boolean)

(defcustom agent-shell-sidebar-no-delete-other-windows t
  "Whether the sidebar window survives `delete-other-windows'."
  :type 'boolean)

(defcustom agent-shell-sidebar-resize-on-open t
  "Whether to resize the sidebar window when opening it."
  :type 'boolean)

(defcustom agent-shell-sidebar-window-fixed 'width
  "Value for `window-size-fixed' in sidebar buffers."
  :type '(choice (const :tag "Not fixed" nil)
                 (const :tag "Fixed width" width)
                 (const :tag "Fixed height" height)))

(defcustom agent-shell-sidebar-parse-chunk-size 30
  "Number of transcript headers to parse per idle tick."
  :type 'integer)

(defcustom agent-shell-sidebar-parse-idle-delay 0.1
  "Idle delay in seconds between parse chunks."
  :type 'number)

(defcustom agent-shell-sidebar-open-file-in-most-recently-used-window t
  "Whether visited chats/transcripts open in the MRU window."
  :type 'boolean)

(defcustom agent-shell-sidebar-refresh-timer 30
  "Auto-refresh the sidebar every N seconds when idle.  Nil disables."
  :type '(choice (const :tag "Disabled" nil) integer))

(defcustom agent-shell-sidebar-extra-project-roots nil
  "Additional project roots to scan.
Merged with `project-known-project-roots' and
`projectile-known-projects'."
  :type '(repeat directory))

(defcustom agent-shell-sidebar-collapse-empty-projects t
  "Hide projects with no transcripts from the sidebar."
  :type 'boolean)

;;; Faces

(defface agent-shell-sidebar-project-face
  '((t :inherit dired-directory :weight bold))
  "Face for project header rows.")

(defface agent-shell-sidebar-date-face
  '((t :inherit font-lock-constant-face))
  "Face for transcript date column.")

(defface agent-shell-sidebar-agent-face
  '((t :inherit font-lock-keyword-face))
  "Face for the agent name column.")

(defface agent-shell-sidebar-model-face
  '((t :inherit font-lock-comment-face))
  "Face for the model column.")

(defface agent-shell-sidebar-mark-face
  '((t :inherit warning))
  "Face for row marks (e.g. delete).")

(defface agent-shell-sidebar-placeholder-face
  '((t :inherit shadow))
  "Face for un-parsed placeholder metadata.")

;;; State

(defconst agent-shell-sidebar--cache-schema 4
  "Bump when the parsed header format changes to invalidate old entries.")

(defvar-local agent-shell-sidebar--parse-cache nil
  "Maps this buffer's paths to schema, file signature, and header plists.
The header plist has :agent :started :session-id :model :cwd :preview.")

(defvar-local agent-shell-sidebar--parse-timer nil
  "Active idle timer for chunked header parsing.")

(defvar-local agent-shell-sidebar--parse-queue nil
  "List of transcript paths awaiting header parse.")

(defvar-local agent-shell-sidebar--parse-generation nil
  "Identity of the current parse queue, invalidated by refresh or cleanup.")

(defvar-local agent-shell-sidebar--marks nil
  "Hash table mapping transcript path -> mark symbol (e.g. `delete').")

(defvar-local agent-shell-sidebar--collapsed nil
  "Hash table of collapsed project roots.")

(defvar-local agent-shell-sidebar--refresh-timer-object nil
  "Per-buffer idle timer for auto-refresh.")

(defvar-local agent-shell-sidebar--groups nil
  "Project and transcript snapshot from the last refresh.")

(defvar-local agent-shell-sidebar--file-info nil
  "Hash table of transcript paths to file attributes from the last refresh.")

(defvar-local agent-shell-sidebar--context-roots nil
  "Directories from which this sidebar was opened.")

(defvar-local agent-shell-sidebar--filter ""
  "Case-insensitive literal filter for transcript metadata and paths.")

(defcustom agent-shell-sidebar-include-remote-projects nil
  "Whether discovery scans remote projects.
Remote directory and file operations can wait for a connection."
  :type 'boolean)

(defvar-local agent-shell-sidebar--pending-count 0
  "Number of headers still queued.")

(defvar-local agent-shell-sidebar--parse-retry-file nil
  "Queue head that has already failed once in this refresh.")

(defvar-local agent-shell-sidebar--render-iterator nil
  "Iterator constructing the next coherent metadata snapshot.")

(defvar-local agent-shell-sidebar--render-buffer nil
  "Staging buffer for the next metadata snapshot.")

(defvar-local agent-shell-sidebar--render-dirty nil
  "Non-nil when metadata changed since the current render began.")

(defvar-local agent-shell-sidebar--render-error nil
  "Rendering error, cleared by a manual redraw or refresh.")

(defvar-local agent-shell-sidebar--project-names nil
  "Disambiguated project names from the last synchronous redraw.")

(defvar agent-shell-sidebar--timer-buffers nil
  "Sidebar buffers whose timers must restart after input.")

(defvar agent-shell-sidebar--resetting-idle nil
  "Non-nil while resetting timer delays for a new idle period.")

;; Derived sidebar modes can share the queue, cache, and window lifecycle.
(defvar-local agent-shell-sidebar--collect-function nil
  "Optional discovery function returning groups and setting file attributes.")
(defvar-local agent-shell-sidebar--parse-file-function nil
  "Optional function that returns metadata for a queued file path.")
(defvar-local agent-shell-sidebar--render-contents-function nil
  "Optional function that inserts the current view without scanning files.")
(defvar-local agent-shell-sidebar--render-snapshot-function nil
  "Optional iterator factory accepting output buffer, groups, and names.")
(defvar-local agent-shell-sidebar--owned-file-p-function nil
  "Optional predicate for additional files owned by live session buffers.")
(defvar-local agent-shell-sidebar--row-identity-properties
  '(agent-shell-sidebar-file agent-shell-sidebar-agent agent-shell-sidebar-project)
  "Text properties identifying actionable rows in this sidebar.")

;;; Discovery

(defun agent-shell-sidebar--regular-file-attributes-p (attrs)
  "Return non-nil when ATTRS describe a regular file, excluding symlinks."
  (and attrs
       (null (file-attribute-type attrs))
       (string-prefix-p "-" (file-attribute-modes attrs))))

(defun agent-shell-sidebar--project-roots ()
  "Return normalized roots from project lists and sidebar context."
  (let ((roots (append
                (when (fboundp 'project-known-project-roots)
                  (project-known-project-roots))
                (when (bound-and-true-p projectile-known-projects)
                  projectile-known-projects)
                agent-shell-sidebar-extra-project-roots
                agent-shell-sidebar--context-roots
                (mapcar (lambda (buffer)
                          (buffer-local-value 'default-directory buffer))
                        (agent-shell-buffers)))))
    (delete-dups
     (delq nil
           (mapcar (lambda (root)
                     (when (and (stringp root) (not (string-empty-p root))
                                (or agent-shell-sidebar-include-remote-projects
                                    (not (file-remote-p root))))
                       (file-name-as-directory (expand-file-name root))))
                   roots)))))

(defun agent-shell-sidebar--transcripts-for-root (root)
  "Return regular transcript files under ROOT, newest first.
Record attributes once for sorting, cache lookup, and display."
  (condition-case nil
      (let (paths)
        (dolist (entry (directory-files-and-attributes
                       (expand-file-name ".agent-shell/transcripts/" root)
                       t "\\.md\\'" t))
          (let ((file (car entry)) (attrs (cdr entry)))
            (when (agent-shell-sidebar--regular-file-attributes-p attrs)
              (puthash file attrs agent-shell-sidebar--file-info)
              (push file paths))))
        (sort paths #'agent-shell-sidebar--newer-p))
    (file-error nil)))

(defun agent-shell-sidebar--mtime (file)
  "Return FILE's modification time from the discovery snapshot."
  (when-let* ((attrs (and agent-shell-sidebar--file-info
                         (gethash file agent-shell-sidebar--file-info))))
    (file-attribute-modification-time attrs)))

(defun agent-shell-sidebar--newer-p (a b)
  "Return non-nil when A precedes B by modification time and then path."
  (let ((ma (agent-shell-sidebar--mtime a))
        (mb (agent-shell-sidebar--mtime b)))
    (cond ((equal ma mb) (string< a b))
          ((null ma) nil)
          ((null mb) t)
          (t (time-less-p mb ma)))))

(defun agent-shell-sidebar--collect ()
  "Collect projects and transcript paths, newest first."
  (setq agent-shell-sidebar--file-info (make-hash-table :test 'equal))
  (let (groups)
    (dolist (root (agent-shell-sidebar--project-roots))
      (let ((paths (agent-shell-sidebar--transcripts-for-root root)))
        (when (or paths (not agent-shell-sidebar-collapse-empty-projects))
          (push (cons root paths) groups))))
    (sort groups
          (lambda (a b)
            (cond ((and (cdr a) (cdr b))
                   (agent-shell-sidebar--newer-p (cadr a) (cadr b)))
                  ((cdr a) t)
                  ((cdr b) nil)
                  (t (string< (car a) (car b))))))))

;;; Header parsing + cache

(defun agent-shell-sidebar--parse-header-from-file (file)
  "Read at most 8192 bytes of FILE and return transcript metadata.
Only fields before the header separator or first message are metadata.
Return an :error field when FILE is not regular, cannot be read, or its
metadata section extends past the byte limit.  Keep complete fields."
  (with-temp-buffer
    (condition-case err
        (let ((attrs (file-attributes file)) after)
          ;; Discovery is only a snapshot: a path may have become a FIFO or
          ;; another special file before its queued parse or activation.
          (unless (agent-shell-sidebar--regular-file-attributes-p attrs)
            (signal 'file-error (list "Not a regular transcript file" file)))
          (insert-file-contents file nil 0 8192)
          (setq after (file-attributes file))
          (unless (agent-shell-sidebar--regular-file-attributes-p after)
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

(defun agent-shell-sidebar--signature (attrs)
  "Return the cache signature for file ATTRS."
  (when attrs
    (list (file-attribute-modification-time attrs)
          (file-attribute-size attrs)
          (file-attribute-inode-number attrs))))

(defun agent-shell-sidebar--cached-header (file &optional attrs)
  "Return FILE's cached header when its signature matches ATTRS.
Use the discovery snapshot when ATTRS is nil.  Do not access the disk."
  (let ((entry (and agent-shell-sidebar--parse-cache
                    (gethash file agent-shell-sidebar--parse-cache)))
        (signature (agent-shell-sidebar--signature
                    (or attrs (and agent-shell-sidebar--file-info
                                   (gethash file agent-shell-sidebar--file-info))))))
    (when (and signature
               (equal (plist-get entry :schema) agent-shell-sidebar--cache-schema)
               (equal (plist-get entry :signature) signature))
      (plist-get entry :header))))

(defun agent-shell-sidebar--ensure-parsed (file)
  "Read FILE's metadata when changed or when a previous read failed."
  (unless agent-shell-sidebar--parse-cache
    (setq agent-shell-sidebar--parse-cache (make-hash-table :test 'equal)))
  (let* ((owner (current-buffer))
         (generation agent-shell-sidebar--parse-generation)
         (cache agent-shell-sidebar--parse-cache)
         (entry (gethash file cache))
         (attrs (condition-case nil (file-attributes file) (file-error nil)))
         (cached (and (agent-shell-sidebar--regular-file-attributes-p attrs)
                      (agent-shell-sidebar--cached-header file attrs))))
    (or (and cached (not (plist-get cached :error)) cached)
        (let ((header (funcall (or agent-shell-sidebar--parse-file-function
                                   #'agent-shell-sidebar--parse-header-from-file)
                               file)))
          ;; File hooks can refresh, replace the mode, or parse this file again.
          ;; A completed obsolete read must not overwrite that newer state.
          (when (and (buffer-live-p owner) (eq (current-buffer) owner)
                     (eq generation agent-shell-sidebar--parse-generation)
                     (eq cache agent-shell-sidebar--parse-cache)
                     (eq entry (gethash file cache)))
            (puthash file (list :schema agent-shell-sidebar--cache-schema
                                :signature (agent-shell-sidebar--signature attrs)
                                :header header)
                     cache)
            (when agent-shell-sidebar--file-info
              (puthash file attrs agent-shell-sidebar--file-info)))
          header))))

;;; Async parse loop

(defun agent-shell-sidebar--queue-uncached (paths)
  "Queue PATHS missing successful cached metadata for this refresh."
  (setq agent-shell-sidebar--parse-generation (make-symbol "sidebar-parse")
        agent-shell-sidebar--parse-queue
        (seq-remove (lambda (file)
                      (let ((header (agent-shell-sidebar--cached-header file)))
                        (and header (not (plist-get header :error)))))
                    paths)
        agent-shell-sidebar--pending-count (length agent-shell-sidebar--parse-queue)
        agent-shell-sidebar--parse-retry-file nil))

(defun agent-shell-sidebar--idle-timer (delay function)
  "Call FUNCTION with the current buffer after another DELAY idle seconds.
Add elapsed idle time so successive chunks also run during continuous idle."
  (run-with-idle-timer
   (time-add (if agent-shell-sidebar--resetting-idle 0
               (or (current-idle-time) 0))
             (max 0.01 delay)) nil
   function (current-buffer)))

(defun agent-shell-sidebar--start-parse-timer ()
  "Schedule the next chunk while parsing or rendering remains."
  (when (and (or agent-shell-sidebar--parse-queue
                 (and (not agent-shell-sidebar--render-error)
                      (or agent-shell-sidebar--render-iterator
                          agent-shell-sidebar--render-dirty)))
             (not (timerp agent-shell-sidebar--parse-timer)))
    (setq agent-shell-sidebar--parse-timer
          (agent-shell-sidebar--idle-timer
           agent-shell-sidebar-parse-idle-delay #'agent-shell-sidebar--parse-tick))))

(defun agent-shell-sidebar--current-parse-p (owner generation)
  "Return non-nil if OWNER still holds the sidebar work in GENERATION."
  (and (buffer-live-p owner) (eq (current-buffer) owner)
       (derived-mode-p 'agent-shell-sidebar-mode)
       (eq generation agent-shell-sidebar--parse-generation)))

(defun agent-shell-sidebar--parse-tick (&optional buffer)
  "Parse and render bounded portions of BUFFER without scanning projects."
  (when (buffer-live-p (or buffer (current-buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (when (derived-mode-p 'agent-shell-sidebar-mode)
        (when (timerp agent-shell-sidebar--parse-timer)
          (cancel-timer agent-shell-sidebar--parse-timer))
        (setq agent-shell-sidebar--parse-timer nil)
        (let ((owner (current-buffer))
              (generation agent-shell-sidebar--parse-generation))
          (unwind-protect
              (let ((n 0) retry)
                (while (and (agent-shell-sidebar--current-parse-p owner generation)
                            agent-shell-sidebar--parse-queue (not retry)
                            (< n (max 1 agent-shell-sidebar-parse-chunk-size))
                            (not (input-pending-p)))
                  (let* ((queue agent-shell-sidebar--parse-queue)
                         (file (car queue)))
                    (condition-case err
                        (agent-shell-sidebar--ensure-parsed file)
                      (error
                       (when (and (agent-shell-sidebar--current-parse-p owner generation)
                                  (eq queue agent-shell-sidebar--parse-queue))
                         (if (equal file agent-shell-sidebar--parse-retry-file)
                             (puthash
                              file
                              (list :schema agent-shell-sidebar--cache-schema
                                    :signature (agent-shell-sidebar--signature
                                                (gethash file agent-shell-sidebar--file-info))
                                    :header (list :error (error-message-string err)))
                              agent-shell-sidebar--parse-cache)
                           (setq agent-shell-sidebar--parse-retry-file file
                                 retry t)))))
                    ;; The head remains pending if a hook replaced or advanced
                    ;; the queue.  A quit also retains it and rearms work below.
                    (when (and (not retry)
                               (agent-shell-sidebar--current-parse-p owner generation)
                               (eq queue agent-shell-sidebar--parse-queue))
                      (pop agent-shell-sidebar--parse-queue)
                      (setq agent-shell-sidebar--parse-retry-file nil
                            agent-shell-sidebar--pending-count
                            (max 0 (1- agent-shell-sidebar--pending-count))
                            agent-shell-sidebar--render-dirty t)))
                  (cl-incf n))
                (when (agent-shell-sidebar--current-parse-p owner generation)
                  (agent-shell-sidebar--render-tick)
                  (agent-shell-sidebar--update-header-line)))
            (when (buffer-live-p owner)
              (with-current-buffer owner
                (when (derived-mode-p 'agent-shell-sidebar-mode)
                  (agent-shell-sidebar--start-parse-timer))))))))))

(defun agent-shell-sidebar--reset-idle-timers ()
  "Restart pending timers after input anywhere in Emacs."
  (let ((agent-shell-sidebar--resetting-idle t))
    (dolist (buffer agent-shell-sidebar--timer-buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (dolist (timer (list agent-shell-sidebar--parse-timer
                              agent-shell-sidebar--refresh-timer-object))
            (when (timerp timer) (cancel-timer timer)))
          (setq agent-shell-sidebar--parse-timer nil
                agent-shell-sidebar--refresh-timer-object nil)
          (agent-shell-sidebar--start-parse-timer)
          (agent-shell-sidebar--schedule-refresh))))))

(defun agent-shell-sidebar--schedule-refresh ()
  "Schedule another refresh for this buffer when auto-refresh is enabled."
  (when (timerp agent-shell-sidebar--refresh-timer-object)
    (cancel-timer agent-shell-sidebar--refresh-timer-object))
  (setq agent-shell-sidebar--refresh-timer-object nil)
  (when (and (numberp agent-shell-sidebar-refresh-timer)
             (> agent-shell-sidebar-refresh-timer 0))
    (setq agent-shell-sidebar--refresh-timer-object
          (agent-shell-sidebar--idle-timer
           agent-shell-sidebar-refresh-timer #'agent-shell-sidebar--refresh-tick))))

(defun agent-shell-sidebar--refresh-tick (buffer)
  "Refresh BUFFER when visible, then schedule its next refresh."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq agent-shell-sidebar--refresh-timer-object nil)
      (when (derived-mode-p 'agent-shell-sidebar-mode)
        (unwind-protect
            (when (and (numberp agent-shell-sidebar-refresh-timer)
                       (> agent-shell-sidebar-refresh-timer 0)
                       (get-buffer-window buffer t))
              (agent-shell-sidebar-refresh))
          (agent-shell-sidebar--schedule-refresh))))))

(defun agent-shell-sidebar--cleanup ()
  "Cancel this buffer's timers and pending work."
  (setq agent-shell-sidebar--parse-generation nil)
  (agent-shell-sidebar--cancel-render)
  (setq agent-shell-sidebar--timer-buffers
        (delq (current-buffer) agent-shell-sidebar--timer-buffers))
  (unless agent-shell-sidebar--timer-buffers
    (remove-hook 'post-command-hook #'agent-shell-sidebar--reset-idle-timers))
  (dolist (timer (list agent-shell-sidebar--parse-timer
                       agent-shell-sidebar--refresh-timer-object))
    (when (timerp timer) (cancel-timer timer)))
  (setq agent-shell-sidebar--parse-timer nil
        agent-shell-sidebar--refresh-timer-object nil
        agent-shell-sidebar--parse-queue nil
        agent-shell-sidebar--pending-count 0
        agent-shell-sidebar--parse-retry-file nil))

;;; Live-buffer + config lookups

(defun agent-shell-sidebar--config-names (config)
  "Return nonempty display names from CONFIG."
  (seq-filter (lambda (name) (and (stringp name) (not (string-empty-p name))))
              (list (map-elt config :mode-line-name) (map-elt config :buffer-name))))

(defun agent-shell-sidebar--configs-for-agent-name (name)
  "Return configurations matching NAME, preferring exact names to substrings.
Compare names case-insensitively.
Never match an empty configuration name."
  (when (and (stringp name) (not (string-empty-p name)))
    (let* ((configs (agent-shell--resolved-agent-configs))
           (folded (downcase name))
           (exact (seq-filter
                   (lambda (config)
                     (member folded (mapcar #'downcase
                                            (agent-shell-sidebar--config-names config))))
                   configs))
           (matches (or exact
                        (seq-filter
                         (lambda (config)
                           (seq-some
                            (lambda (candidate)
                              (let ((candidate (downcase candidate)))
                                (or (string-search folded candidate)
                                    (string-search candidate folded))))
                            (agent-shell-sidebar--config-names config)))
                         configs))))
      matches)))

(defun agent-shell-sidebar--config-for-agent-name (name)
  "Return an unambiguous configuration matching NAME."
  (let ((matches (agent-shell-sidebar--configs-for-agent-name name)))
    (when (= (length matches) 1) (car matches))))

(defun agent-shell-sidebar--live-buffer-for-session (session-id &optional agent-name cwd config)
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
                                             (agent-shell-sidebar--config-names
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

(defun agent-shell-sidebar--root-for-file (file)
  "Return FILE's discovered project root, or infer its transcript layout."
  (or (car (seq-find (lambda (group) (member file (cdr group)))
                    agent-shell-sidebar--groups))
      (when (string-match "\\`\\(.*?/\\)\\.agent-shell/transcripts/[^/]+\\'" file)
        (match-string 1 file))
      (user-error "Cannot determine project for %s" file)))

(defun agent-shell-sidebar--working-directory (file header)
  "Resolve FILE's recorded working directory from HEADER.
Use its project root only when the transcript has no recorded directory."
  (let* ((root (agent-shell-sidebar--root-for-file file))
         (cwd (plist-get header :cwd))
         (directory (file-name-as-directory (expand-file-name (or cwd root) root))))
    (unless (file-directory-p directory)
      (user-error "Transcript directory no longer exists: %s" directory))
    directory))

(defun agent-shell-sidebar--select-config (name)
  "Resolve NAME or ask which agent to use when no unique match exists."
  (or (agent-shell-sidebar--config-for-agent-name name)
      (and (null name) (agent-shell--auto-preferred-config))
      (agent-shell-select-config
       :prompt (if name (format "Agent for %s: " name) "Start with agent: "))
      (user-error "No agent selected")))

(defun agent-shell-sidebar--start (directory config &optional session-id strategy)
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

;;; Rendering

(defconst agent-shell-sidebar--placeholder
  (propertize "…" 'face 'agent-shell-sidebar-placeholder-face))

(defun agent-shell-sidebar--short-date (started file)
  "Return a short date from STARTED or FILE's recorded modification time."
  (or (and started
           (string-match "^[0-9]\\{4\\}-\\([0-9]\\{2\\}-[0-9]\\{2\\}\\)[ T]\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)" started)
           (concat (match-string 1 started) " " (match-string 2 started)))
      (when-let* ((mtime (agent-shell-sidebar--mtime file)))
        (format-time-string "%m-%d %H:%M" mtime))
      "??-?? ??:??"))

(defun agent-shell-sidebar--disambiguated-names (roots)
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

(defun agent-shell-sidebar--render-project-header (root count collapsed name)
  "Insert a project header line for ROOT with COUNT transcripts.
COLLAPSED is non-nil when the project section is folded.
NAME is the disambiguated display name."
  (let* ((arrow (if collapsed "▸" "▾"))
         (label (format "%s %s (%d)" arrow name count))
         (start (point)))
    (insert (propertize label 'face 'agent-shell-sidebar-project-face))
    (add-text-properties
     start (point)
     `(agent-shell-sidebar-project ,root
       agent-shell-sidebar-collapsed ,collapsed
       help-echo ,root))
    (insert "\n")))

(defun agent-shell-sidebar--render-agent-header (root agent count collapsed)
  "Insert an agent header line under a project.
ROOT is the project root, AGENT is the agent name (or nil for unknown),
COUNT is the number of transcripts, COLLAPSED is non-nil when folded."
  (let* ((arrow (if collapsed "▸" "▾"))
         (label (format "  %s %s (%d)"
                        arrow
                        (or agent "Unknown")
                        count))
         (start (point)))
    (insert (propertize label 'face 'agent-shell-sidebar-agent-face))
    (add-text-properties
     start (point)
     `(agent-shell-sidebar-agent (,root . ,agent)
       agent-shell-sidebar-collapsed ,collapsed))
    (insert "\n")))

(defun agent-shell-sidebar--group-by-agent (paths)
  "Group sorted PATHS by agent while preserving their newest-first order."
  (let ((groups (make-hash-table :test 'equal)) keys)
    (dolist (path paths)
      (let ((agent (plist-get (agent-shell-sidebar--cached-header path) :agent)))
        (unless (gethash agent groups) (push agent keys))
        (push path (gethash agent groups))))
    (mapcar (lambda (key) (cons key (nreverse (gethash key groups))))
            (nreverse keys))))

(defun agent-shell-sidebar--matches-filter-p (file)
  "Return non-nil when FILE's metadata matches the current filter.
Keep unparsed files visible until their metadata is available."
  (let ((header (agent-shell-sidebar--cached-header file)))
    (or (string-empty-p agent-shell-sidebar--filter)
        (null header)
        (string-search
         (downcase agent-shell-sidebar--filter)
         (downcase (mapconcat
                    #'identity
                    (delq nil (list file (plist-get header :preview)
                                    (plist-get header :agent) (plist-get header :model)
                                    (plist-get header :session-id) (plist-get header :cwd)))
                    "\n"))))))

(defun agent-shell-sidebar--render-transcript-row (file mark)
  "Insert one transcript row for FILE with optional MARK.
Rows are indented to sit under the agent header they belong to."
  (let* ((header (agent-shell-sidebar--cached-header file))
         (parsed (not (null header)))
         (date (if parsed
                   (propertize
                    (agent-shell-sidebar--short-date
                     (plist-get header :started) file)
                    'face 'agent-shell-sidebar-date-face)
                 (propertize (agent-shell-sidebar--short-date nil file)
                             'face 'agent-shell-sidebar-placeholder-face)))
         (preview (and parsed (plist-get header :preview)))
         (error-text (plist-get header :error))
         (mark-str (if (eq mark 'delete)
                       (propertize "D" 'face 'agent-shell-sidebar-mark-face)
                     " "))
         (start (point)))
    (insert "    " mark-str " " date
            (cond
             (error-text " [unreadable transcript]")
             (preview (concat ": " preview))
             (parsed (concat ": " (file-name-base file)))
             (t (concat ": " agent-shell-sidebar--placeholder))))
    (add-text-properties
     start (point)
     `(agent-shell-sidebar-file ,file
       help-echo ,(if header
                      (format "%s\nAgent: %s%s%s%s"
                              file
                              (or (plist-get header :agent) "?")
                              (if (plist-get header :model)
                                  (format "\nModel: %s"
                                          (plist-get header :model))
                                "")
                              (if (plist-get header :session-id)
                                  (format "\nSession: %s"
                                          (plist-get header :session-id))
                                "")
                              (if (plist-get header :preview)
                                  (format "\n\n%s"
                                          (plist-get header :preview))
                                ""))
                    file)))
    (insert "\n")))

(defun agent-shell-sidebar--row-location (position)
  "Record POSITION by row identity, line number, and column."
  (save-excursion
    (goto-char position)
    (list (seq-some
           (lambda (property)
             (when-let* ((value (get-text-property (line-beginning-position) property)))
               (cons property value)))
           agent-shell-sidebar--row-identity-properties)
          (line-number-at-pos) (current-column))))

(defun agent-shell-sidebar--restore-location (location)
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

(defun agent-shell-sidebar--redraw ()
  "Redraw the current sidebar buffer, preserving point on the same file/project."
  (when (derived-mode-p 'agent-shell-sidebar-mode)
    (agent-shell-sidebar--cancel-render)
    (setq agent-shell-sidebar--render-error nil)
    (let* ((location (agent-shell-sidebar--row-location (point)))
           (views (mapcar
                   (lambda (window)
                     (list window
                           (agent-shell-sidebar--row-location (window-start window))
                           (agent-shell-sidebar--row-location (window-point window))))
                   (get-buffer-window-list (current-buffer) nil t)))
           (inhibit-read-only t)
           (groups agent-shell-sidebar--groups))
      (erase-buffer)
      (unless agent-shell-sidebar--marks
        (setq agent-shell-sidebar--marks (make-hash-table :test 'equal)))
      (unless agent-shell-sidebar--collapsed
        (setq agent-shell-sidebar--collapsed (make-hash-table :test 'equal)))
      (if agent-shell-sidebar--render-contents-function
          (funcall agent-shell-sidebar--render-contents-function)
        (if (null groups)
            (insert (propertize "  (no transcripts found)\n"
                                'face 'agent-shell-sidebar-placeholder-face))
          (let ((names (agent-shell-sidebar--disambiguated-names
                        (mapcar #'car groups))))
            (setq agent-shell-sidebar--project-names names)
            (dolist (group groups)
              (let* ((root (car group))
                     (paths (seq-filter #'agent-shell-sidebar--matches-filter-p (cdr group)))
                     (project-collapsed
                      (gethash root agent-shell-sidebar--collapsed))
                     (name (or (gethash root names)
                               (file-name-nondirectory
                                (directory-file-name root)))))
                (when (or paths (string-empty-p agent-shell-sidebar--filter))
                  (agent-shell-sidebar--render-project-header
                   root (length paths) project-collapsed name))
                (unless project-collapsed
                  (dolist (agent-group
                           (agent-shell-sidebar--group-by-agent paths))
                    (let* ((agent (car agent-group))
                           (agent-paths (cdr agent-group))
                           (agent-key (cons root agent))
                           (agent-collapsed
                            (gethash agent-key
                                     agent-shell-sidebar--collapsed)))
                      (agent-shell-sidebar--render-agent-header
                       root agent (length agent-paths) agent-collapsed)
                      (unless agent-collapsed
                        (dolist (p agent-paths)
                          (agent-shell-sidebar--render-transcript-row
                           p (gethash p agent-shell-sidebar--marks))))))))))))
      (when (= (point-min) (point-max))
        (insert "  (no matching transcripts)\n"))
      (agent-shell-sidebar--update-header-line)
      (dolist (view views)
        (when (window-live-p (car view))
          (agent-shell-sidebar--restore-location (cadr view))
          (set-window-start (car view) (line-beginning-position) t)
          (agent-shell-sidebar--restore-location (caddr view))
          (set-window-point (car view) (point))))
      (agent-shell-sidebar--restore-location location))))

(defun agent-shell-sidebar--update-header-line ()
  "Update controls and work status without traversing the parse queue."
  (setq header-line-format
        (format " RET resume  o read  TAB fold  / filter  N new  g refresh%s%s%s"
                (if (string-empty-p agent-shell-sidebar--filter) ""
                  (concat " | " (replace-regexp-in-string "%" "%%" agent-shell-sidebar--filter)))
                (if agent-shell-sidebar--parse-queue
                    (format " | %d pending" agent-shell-sidebar--pending-count) "")
                (cond (agent-shell-sidebar--render-error " | redraw failed; g retries")
                      (agent-shell-sidebar--render-iterator " | updating view")
                      (t "")))))

(defun agent-shell-sidebar--cancel-render ()
  "Discard any unpublished snapshot and pending render request."
  (setq agent-shell-sidebar--render-iterator nil
        agent-shell-sidebar--render-dirty nil)
  (when (buffer-live-p agent-shell-sidebar--render-buffer)
    (kill-buffer agent-shell-sidebar--render-buffer))
  (setq agent-shell-sidebar--render-buffer nil))

(defun agent-shell-sidebar--append-item (queue item)
  "Append ITEM to the (HEAD . TAIL) QUEUE in constant time."
  (let ((cell (list item)))
    (if (cdr queue) (setcdr (cdr queue) cell) (setcar queue cell))
    (setcdr queue cell)))

(iter-defun agent-shell-sidebar--render-snapshot (output groups names)
  "Build a coherent view in OUTPUT, yielding after each path or row.
GROUPS and NAMES come from the last refresh.  Capture metadata as each
path is considered, so grouping and displayed rows use the same header."
  (let ((cache (make-hash-table :test 'equal))
        (info (make-hash-table :test 'equal))
        (marks agent-shell-sidebar--marks))
    (dolist (group groups)
      (let* ((root (car group))
             (collapsed (gethash root agent-shell-sidebar--collapsed))
             (by-agent (make-hash-table :test 'equal))
             (agents (cons nil nil))
             (count 0))
        (iter-yield nil)
        (dolist (path (cdr group))
          (puthash path (gethash path agent-shell-sidebar--parse-cache) cache)
          (puthash path (gethash path agent-shell-sidebar--file-info) info)
          (when (agent-shell-sidebar--matches-filter-p path)
            (cl-incf count)
            (unless collapsed
              (let* ((agent (plist-get (agent-shell-sidebar--cached-header path) :agent))
                     (entry (gethash agent by-agent)))
                (unless entry
                  ;; Each entry holds its name, count and a path queue.
                  (setq entry (vector agent 0 (cons nil nil)))
                  (puthash agent entry by-agent)
                  (agent-shell-sidebar--append-item agents entry))
                (cl-incf (aref entry 1))
                (agent-shell-sidebar--append-item (aref entry 2) path))))
          (iter-yield nil))
        (when (or (> count 0) (string-empty-p agent-shell-sidebar--filter))
          (with-current-buffer output
            (agent-shell-sidebar--render-project-header
             root count collapsed (gethash root names)))
          (iter-yield nil))
        (dolist (entry (car agents))
          (let* ((agent (aref entry 0))
                 (folded (gethash (cons root agent) agent-shell-sidebar--collapsed)))
            (with-current-buffer output
              (agent-shell-sidebar--render-agent-header root agent (aref entry 1) folded))
            (iter-yield nil)
            (unless folded
              (dolist (path (car (aref entry 2)))
                (with-current-buffer output
                  (let ((agent-shell-sidebar--parse-cache cache)
                        (agent-shell-sidebar--file-info info))
                    (agent-shell-sidebar--render-transcript-row path (gethash path marks))))
                (iter-yield nil)))))))
    (with-current-buffer output
      (when (= (buffer-size) 0)
        (insert (if groups "  (no matching transcripts)\n"
                  (propertize "  (no transcripts found)\n"
                              'face 'agent-shell-sidebar-placeholder-face)))))))

(defun agent-shell-sidebar--publish-render ()
  "Publish the completed snapshot, preserving each current window view."
  (let ((inhibit-quit t)
        (location (agent-shell-sidebar--row-location (point)))
        (views (mapcar
                (lambda (window)
                  (list window
                        (agent-shell-sidebar--row-location (window-start window))
                        (agent-shell-sidebar--row-location (window-point window))))
                (get-buffer-window-list (current-buffer) nil t))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-buffer-substring agent-shell-sidebar--render-buffer))
    (dolist (view views)
      (when (window-live-p (car view))
        (agent-shell-sidebar--restore-location (cadr view))
        (set-window-start (car view) (line-beginning-position) t)
        (agent-shell-sidebar--restore-location (caddr view))
        (set-window-point (car view) (point))))
    (agent-shell-sidebar--restore-location location))
  (kill-buffer agent-shell-sidebar--render-buffer)
  (setq agent-shell-sidebar--render-buffer nil
        agent-shell-sidebar--render-iterator nil))

(defun agent-shell-sidebar--render-tick ()
  "Advance at most 300 path or row operations, coalescing parse updates.
Publication copies a completed buffer and restores row locations."
  (unless agent-shell-sidebar--render-error
    (condition-case err
        (progn
          (when (and agent-shell-sidebar--render-dirty
                     (not agent-shell-sidebar--render-iterator))
            (setq agent-shell-sidebar--render-dirty nil
                  agent-shell-sidebar--render-buffer
                  (generate-new-buffer " *agent-shell-sidebar render*")
                  agent-shell-sidebar--render-iterator
                  (funcall (or agent-shell-sidebar--render-snapshot-function
                               #'agent-shell-sidebar--render-snapshot)
                           agent-shell-sidebar--render-buffer agent-shell-sidebar--groups
                           agent-shell-sidebar--project-names)))
          (let ((n 0))
            (while (and agent-shell-sidebar--render-iterator (< n 300)
                        (not (input-pending-p)))
              (condition-case nil
                  (iter-next agent-shell-sidebar--render-iterator)
                (iter-end-of-sequence (agent-shell-sidebar--publish-render)))
              (cl-incf n))))
      (error
       (agent-shell-sidebar--cancel-render)
       (setq agent-shell-sidebar--render-error (error-message-string err))
       (message "Sidebar redraw failed: %s" agent-shell-sidebar--render-error))
      (quit
       (agent-shell-sidebar--cancel-render)
       (setq agent-shell-sidebar--render-dirty t)
       (signal (car err) (cdr err)))))
  (when agent-shell-sidebar--render-error
    (setq agent-shell-sidebar--render-dirty nil)))

;;; Mode + keymap

(defvar agent-shell-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agent-shell-sidebar-visit)
    (define-key map (kbd "o") #'agent-shell-sidebar-open-transcript)
    (define-key map (kbd "/") #'agent-shell-sidebar-set-filter)
    (define-key map (kbd "N") #'agent-shell-sidebar-new-session)
    (define-key map (kbd "g") #'agent-shell-sidebar-refresh)
    (define-key map (kbd "q") #'agent-shell-sidebar-hide-sidebar)
    (define-key map (kbd "n") #'agent-shell-sidebar-next-line)
    (define-key map (kbd "p") #'agent-shell-sidebar-previous-line)
    (define-key map (kbd "d") #'agent-shell-sidebar-mark-delete)
    (define-key map (kbd "u") #'agent-shell-sidebar-unmark)
    (define-key map (kbd "U") #'agent-shell-sidebar-unmark-all)
    (define-key map (kbd "x") #'agent-shell-sidebar-execute)
    (define-key map (kbd "TAB") #'agent-shell-sidebar-toggle-project)
    (define-key map (kbd "<tab>") #'agent-shell-sidebar-toggle-project)
    (define-key map [mouse-2] #'agent-shell-sidebar-mouse-visit)
    map)
  "Keymap for `agent-shell-sidebar-mode'.")

(define-derived-mode agent-shell-sidebar-mode special-mode "AgentChats"
  "Browse agent-shell transcripts grouped by project and agent.
RET resumes a session or toggles a section.  The o key reads a transcript.
The / key filters metadata.  Use a prefix argument to clear the filter.
The N key starts a session in the project at point.
The g key refreshes files and the q key hides the window.
The d key marks a transcript, and the u key removes its mark.
The x key deletes marked transcripts after confirmation.
\\{agent-shell-sidebar-mode-map}"
  :group 'agent-shell-sidebar
  (setq-local truncate-lines t)
  (setq-local window-size-fixed agent-shell-sidebar-window-fixed)
  (setq-local cursor-in-non-selected-windows nil)
  (setq-local agent-shell-sidebar--parse-cache (make-hash-table :test 'equal))
  (setq-local agent-shell-sidebar--marks (make-hash-table :test 'equal))
  (setq-local agent-shell-sidebar--collapsed (make-hash-table :test 'equal))
  (cl-pushnew (current-buffer) agent-shell-sidebar--timer-buffers)
  (add-hook 'post-command-hook #'agent-shell-sidebar--reset-idle-timers)
  (add-hook 'kill-buffer-hook #'agent-shell-sidebar--cleanup nil t)
  (add-hook 'change-major-mode-hook #'agent-shell-sidebar--cleanup nil t)
  (agent-shell-sidebar--schedule-refresh))

;;; Commands: navigation

(defun agent-shell-sidebar-next-line (&optional n)
  "Move N actionable rows without leaving the last row."
  (interactive "p")
  (let ((step (if (< (or n 1) 0) -1 1)))
    (dotimes (_ (abs (or n 1)))
      (let ((old (point)))
        (forward-line step)
        (unless (seq-some (lambda (property) (get-text-property (point) property))
                          agent-shell-sidebar--row-identity-properties)
          (goto-char old)))))
  (beginning-of-line))

(defun agent-shell-sidebar-previous-line (&optional n)
  "Move N actionable rows backward."
  (interactive "p")
  (agent-shell-sidebar-next-line (- (or n 1))))

;;; Commands: activation

(defun agent-shell-sidebar--pop-to (buffer)
  "Display BUFFER in an ordinary window and select it."
  (let* ((windows (seq-filter
                   (lambda (window)
                     (and (not (window-dedicated-p window))
                          (not (window-parameter window 'window-side))))
                   (window-list nil 'no-minibuffer)))
         (window (if agent-shell-sidebar-open-file-in-most-recently-used-window
                     (car (sort windows (lambda (a b) (> (window-use-time a) (window-use-time b)))))
                   (car windows))))
    (if window
        (progn (set-window-buffer window buffer) (select-window window))
      (pop-to-buffer buffer '((display-buffer-pop-up-window display-buffer-pop-up-frame))))))

(defun agent-shell-sidebar--visit-header (file header &optional strategy)
  "Visit FILE using validated HEADER and optional startup STRATEGY."
  (when (plist-get header :error) (user-error "%s" (plist-get header :error)))
  (let* ((session-id (plist-get header :session-id))
         (name (plist-get header :agent))
         (directory (agent-shell-sidebar--working-directory file header))
         (existing (and name
                        (agent-shell-sidebar--live-buffer-for-session
                         session-id name directory)))
         ;; A removed configuration must not prevent access to its live shell.
         ;; Configured ambiguous matches still require explicit selection.
         (unconfigured (and existing
                            (null (agent-shell-sidebar--configs-for-agent-name name))))
         (config (unless unconfigured (agent-shell-sidebar--select-config name)))
         (live (if unconfigured existing
                 (agent-shell-sidebar--live-buffer-for-session session-id name directory config))))
    (agent-shell-sidebar--pop-to
     (or live (if strategy
                  (agent-shell-sidebar--start directory config session-id strategy)
                (agent-shell-sidebar--start directory config session-id))))))

(defun agent-shell-sidebar-visit ()
  "Toggle a header, reuse a session buffer, or resume the transcript at point."
  (interactive)
  (cond
   ((or (get-text-property (line-beginning-position) 'agent-shell-sidebar-project)
        (get-text-property (line-beginning-position) 'agent-shell-sidebar-agent))
    (agent-shell-sidebar-toggle-project))
   ((get-text-property (line-beginning-position) 'agent-shell-sidebar-file)
    (let* ((file (get-text-property (line-beginning-position) 'agent-shell-sidebar-file))
           (header (agent-shell-sidebar--ensure-parsed file)))
      (agent-shell-sidebar--visit-header file header)))
   (t (user-error "No transcript at point"))))

(defun agent-shell-sidebar-new-session ()
  "Start a new session in the project at point, using its agent when known."
  (interactive)
  (let* ((file (get-text-property (line-beginning-position) 'agent-shell-sidebar-file))
         (agent (get-text-property (line-beginning-position) 'agent-shell-sidebar-agent))
         (root (or (get-text-property (line-beginning-position) 'agent-shell-sidebar-project)
                   (car agent) (and file (agent-shell-sidebar--root-for-file file))))
         (name (or (cdr agent) (and file (plist-get (agent-shell-sidebar--ensure-parsed file) :agent)))))
    (unless (and root (file-directory-p root)) (user-error "No existing project at point"))
    (agent-shell-sidebar--pop-to
     (agent-shell-sidebar--start root (agent-shell-sidebar--select-config name)))))

(defun agent-shell-sidebar-mouse-visit (event)
  "Select the clicked window and activate the row for EVENT."
  (interactive "e")
  (mouse-set-point event)
  (agent-shell-sidebar-visit))

(defun agent-shell-sidebar-open-transcript ()
  "Open the transcript at point in View mode."
  (interactive)
  (let ((file (get-text-property (line-beginning-position) 'agent-shell-sidebar-file)))
    (unless file (user-error "No transcript at point"))
    (unless (file-readable-p file) (user-error "Cannot read transcript: %s" file))
    (unless (agent-shell-sidebar--regular-file-attributes-p (file-attributes file))
      (user-error "Not a regular transcript file: %s" file))
    (let ((buffer (find-file-noselect file)))
      (with-current-buffer buffer (view-mode 1))
      (agent-shell-sidebar--pop-to buffer))))

(defun agent-shell-sidebar-set-filter (text)
  "Filter rows by literal TEXT.  With a prefix argument, clear the filter."
  (interactive (list (if current-prefix-arg ""
                       (read-string "Filter transcripts: " agent-shell-sidebar--filter))))
  (setq agent-shell-sidebar--filter (string-trim text))
  (agent-shell-sidebar--redraw))

(defun agent-shell-sidebar-toggle-project ()
  "Fold or unfold the project or agent section at point."
  (interactive)
  (let ((key (or (get-text-property (line-beginning-position) 'agent-shell-sidebar-project)
                 (get-text-property (line-beginning-position) 'agent-shell-sidebar-agent))))
    (unless key (user-error "Not on a project or agent header"))
    (if (gethash key agent-shell-sidebar--collapsed)
        (remhash key agent-shell-sidebar--collapsed)
      (puthash key t agent-shell-sidebar--collapsed))
    (agent-shell-sidebar--redraw)))

;;; Commands: marks

(defun agent-shell-sidebar-mark-delete ()
  "Mark the transcript at point for deletion."
  (interactive)
  (let ((file (get-text-property (line-beginning-position) 'agent-shell-sidebar-file)))
    (unless file (user-error "No transcript at point"))
    (puthash file 'delete agent-shell-sidebar--marks)
    (agent-shell-sidebar--redraw)
    (agent-shell-sidebar-next-line 1)))

(defun agent-shell-sidebar-unmark ()
  "Remove any mark from the transcript at point."
  (interactive)
  (let ((file (get-text-property (line-beginning-position) 'agent-shell-sidebar-file)))
    (unless file (user-error "No transcript at point"))
    (remhash file agent-shell-sidebar--marks)
    (agent-shell-sidebar--redraw)
    (agent-shell-sidebar-next-line 1)))

(defun agent-shell-sidebar-unmark-all ()
  "Remove all deletion marks."
  (interactive)
  (clrhash agent-shell-sidebar--marks)
  (agent-shell-sidebar--redraw))

(defun agent-shell-sidebar-execute ()
  "Delete marked transcripts after confirmation, retaining failed marks.
Refuse deletion of transcripts owned by existing agent-shell buffers."
  (interactive)
  (let (files failures (deleted 0))
    (maphash (lambda (path mark) (when (eq mark 'delete) (push path files)))
             agent-shell-sidebar--marks)
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
              (when (and agent-shell-sidebar--owned-file-p-function
                         (funcall agent-shell-sidebar--owned-file-p-function file))
                (user-error "Transcript belongs to an open session: %s" file))
              (when (file-exists-p file) (delete-file file delete-by-moving-to-trash))
              (cl-incf deleted)
              (remhash file agent-shell-sidebar--parse-cache)
              (remhash file agent-shell-sidebar--marks))
          (file-error (push (error-message-string err) failures))
          (user-error (push (error-message-string err) failures))))
      (agent-shell-sidebar-refresh)
      (if failures
          (message "Deleted %d. Retained %d mark(s): %s"
                   deleted (length failures) (string-join (nreverse failures) "; "))
        (message "Deleted %d transcript(s)" deleted))))))

;;; Commands: refresh

(defun agent-shell-sidebar-refresh ()
  "Rescan transcripts once and schedule uncached metadata for parsing."
  (interactive)
  (unless (derived-mode-p 'agent-shell-sidebar-mode)
    (user-error "Not in an agent-shell sidebar"))
  ;; Publish discovery only after it succeeds, preserving pending work on error.
  (let* ((snapshot
          (let ((agent-shell-sidebar--file-info nil))
            (list (funcall (or agent-shell-sidebar--collect-function
                               #'agent-shell-sidebar--collect))
                  agent-shell-sidebar--file-info))))
    (when (timerp agent-shell-sidebar--parse-timer)
      (cancel-timer agent-shell-sidebar--parse-timer))
    (setq agent-shell-sidebar--parse-timer nil
          agent-shell-sidebar--groups (car snapshot)
          agent-shell-sidebar--file-info (cadr snapshot)))
  (agent-shell-sidebar--queue-uncached
   (apply #'append (mapcar #'cdr agent-shell-sidebar--groups)))
  (agent-shell-sidebar--redraw)
  (agent-shell-sidebar--start-parse-timer))

;;; Sidebar window commands

(defun agent-shell-sidebar--sidebar-buffer (&optional frame)
  "Return a sidebar window in FRAME, or the selected frame when nil."
  (seq-find (lambda (window)
              (and (window-parameter window 'window-side)
                   (with-current-buffer (window-buffer window)
                     (derived-mode-p 'agent-shell-sidebar-mode))))
            (window-list frame 'no-minibuffer)))

(defun agent-shell-sidebar-showing-sidebar-p ()
  "Non-nil if the sidebar is visible in the selected frame."
  (agent-shell-sidebar--sidebar-buffer))

(defun agent-shell-sidebar--get-or-create-buffer ()
  "Return an initialized sidebar buffer without overwriting another buffer."
  (let ((buffer (get-buffer-create agent-shell-sidebar-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-sidebar-mode)
        (unless (= (buffer-size) 0)
          (user-error "Buffer %s is already in use" agent-shell-sidebar-name))
        (agent-shell-sidebar-mode)))
    buffer))

(defun agent-shell-sidebar--set-width (width)
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
(defun agent-shell-sidebar-show-sidebar ()
  "Show and refresh the sidebar, including the current directory and project."
  (interactive)
  (let* ((directory default-directory)
         (project (unless (file-remote-p directory) (project-current nil)))
         (roots (delq nil (list directory (and project (project-root project)))))
         (buffer (agent-shell-sidebar--get-or-create-buffer))
         (window (with-current-buffer buffer
                   ;; An ordinary view of this buffer must permit the side split.
                   (let ((window-size-fixed nil))
                     (display-buffer-in-side-window
                      buffer (append agent-shell-sidebar-display-alist
                                     `((window-width . ,agent-shell-sidebar-width))))))))
    (with-current-buffer buffer
      (setq agent-shell-sidebar--context-roots
            (delete-dups (append roots agent-shell-sidebar--context-roots)))
      (agent-shell-sidebar-refresh))
    (when window
      (set-window-dedicated-p window t)
      (set-window-parameter window 'no-delete-other-windows
                            agent-shell-sidebar-no-delete-other-windows)
      (when agent-shell-sidebar-resize-on-open
        (with-selected-window window
          (condition-case nil
              (agent-shell-sidebar--set-width agent-shell-sidebar-width)
            (error nil)))))))

;;;###autoload
(defun agent-shell-sidebar-hide-sidebar ()
  "Hide the agent-shell chat sidebar in the selected frame."
  (interactive)
  (when-let* ((win (agent-shell-sidebar--sidebar-buffer)))
    (delete-window win)))

;;;###autoload
(defun agent-shell-sidebar-toggle-sidebar ()
  "Toggle the agent-shell chat sidebar."
  (interactive)
  (if (agent-shell-sidebar-showing-sidebar-p)
      (agent-shell-sidebar-hide-sidebar)
    (agent-shell-sidebar-show-sidebar)
    (when agent-shell-sidebar-pop-to-sidebar-on-toggle-open
      (when-let* ((win (agent-shell-sidebar--sidebar-buffer)))
        (select-window win)))))

;;;###autoload
(defun agent-shell-sidebar-jump-to-sidebar ()
  "Select the sidebar window, showing it first if hidden."
  (interactive)
  (unless (agent-shell-sidebar-showing-sidebar-p)
    (agent-shell-sidebar-show-sidebar))
  (when-let* ((window (agent-shell-sidebar--sidebar-buffer)))
    (select-window window)))

;;; Evil bindings

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal agent-shell-sidebar-mode-map
      (kbd "RET") #'agent-shell-sidebar-visit
      (kbd "o")   #'agent-shell-sidebar-open-transcript
      (kbd "/")   #'agent-shell-sidebar-set-filter
      (kbd "N")   #'agent-shell-sidebar-new-session
      (kbd "gr")  #'agent-shell-sidebar-refresh
      (kbd "gg")  #'evil-goto-first-line
      (kbd "G")   #'evil-goto-line
      (kbd "j")   #'agent-shell-sidebar-next-line
      (kbd "k")   #'agent-shell-sidebar-previous-line
      (kbd "d")   #'agent-shell-sidebar-mark-delete
      (kbd "u")   #'agent-shell-sidebar-unmark
      (kbd "U")   #'agent-shell-sidebar-unmark-all
      (kbd "x")   #'agent-shell-sidebar-execute
      (kbd "TAB") #'agent-shell-sidebar-toggle-project
      (kbd "^")   #'agent-shell-sidebar-toggle-project
      (kbd "-")   #'agent-shell-sidebar-toggle-project
      (kbd "q")   #'agent-shell-sidebar-hide-sidebar
      (kbd "ZZ")  #'agent-shell-sidebar-hide-sidebar
      (kbd "ZQ")  #'agent-shell-sidebar-hide-sidebar))
  (when (fboundp 'evil-make-overriding-map)
    (evil-make-overriding-map agent-shell-sidebar-mode-map 'normal)))

(provide 'agent-shell-sidebar)
;;; agent-shell-sidebar.el ends here
