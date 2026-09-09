;;; agent-shell-sidebar.el --- Sidebar browser for agent-shell chats -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Keywords: agent-shell, tools
;; Package-Requires: ((emacs "29.1") (agent-shell "0.60") (project "0.9"))

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
;; are cached keyed by (path . mtime) plus a schema version so parser
;; changes automatically invalidate old entries.  Projects with clashing
;; basenames are disambiguated by prepending parent path components until
;; unique.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'project)
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

(defconst agent-shell-sidebar--cache-schema 2
  "Bump when the parsed header format changes to invalidate old entries.")

(defvar agent-shell-sidebar--parse-cache (make-hash-table :test 'equal)
  "Maps transcript path -> (:schema INT :mtime FLOAT :header PLIST).
The header plist has :agent :started :session-id :model :cwd :preview.")

(defvar agent-shell-sidebar--parse-timer nil
  "Active idle timer for chunked header parsing.")

(defvar agent-shell-sidebar--parse-queue nil
  "List of transcript paths awaiting header parse.")

(defvar-local agent-shell-sidebar--marks nil
  "Hash table mapping transcript path -> mark symbol (e.g. `delete').")

(defvar-local agent-shell-sidebar--collapsed nil
  "Hash table of collapsed project roots.")

(defvar-local agent-shell-sidebar--refresh-timer-object nil
  "Per-buffer idle timer for auto-refresh.")

;;; Discovery

(defun agent-shell-sidebar--project-roots ()
  "Return the union of known project roots, deduplicated and normalized."
  (let ((roots (append
                (when (fboundp 'project-known-project-roots)
                  (project-known-project-roots))
                (when (bound-and-true-p projectile-known-projects)
                  projectile-known-projects)
                agent-shell-sidebar-extra-project-roots)))
    (thread-last roots
                 (mapcar (lambda (r) (when r (expand-file-name (file-name-as-directory r)))))
                 (delq nil)
                 (seq-uniq))))

(defun agent-shell-sidebar--transcripts-for-root (root)
  "Return transcript file paths under project ROOT."
  (let ((dir (expand-file-name ".agent-shell/transcripts/" root)))
    (when (file-directory-p dir)
      (ignore-errors
        (directory-files dir t "\\.md\\'" t)))))

(defun agent-shell-sidebar--collect ()
  "Return alist ((ROOT . (PATH ...)) ...) sorted by transcript mtime desc.
Only roots with at least one transcript are returned when
`agent-shell-sidebar-collapse-empty-projects' is non-nil."
  (let (result)
    (dolist (root (agent-shell-sidebar--project-roots))
      (let ((paths (agent-shell-sidebar--transcripts-for-root root)))
        (when (or paths (not agent-shell-sidebar-collapse-empty-projects))
          (push (cons root
                      (sort paths
                            (lambda (a b)
                              (time-less-p
                               (file-attribute-modification-time (file-attributes b))
                               (file-attribute-modification-time (file-attributes a))))))
                result))))
    (sort result
          (lambda (a b)
            (let ((ma (car (mapcar
                            (lambda (p)
                              (file-attribute-modification-time (file-attributes p)))
                            (cdr a))))
                  (mb (car (mapcar
                            (lambda (p)
                              (file-attribute-modification-time (file-attributes p)))
                            (cdr b)))))
              (cond
               ((and (null ma) (null mb)) (string< (car a) (car b)))
               ((null ma) nil)
               ((null mb) t)
               (t (time-less-p mb ma))))))))

;;; Header parsing + cache

(defun agent-shell-sidebar--parse-header-from-file (file)
  "Parse the transcript header from FILE and return a plist.
Reads only the first ~8KB.  Returns a plist with keys
:agent :started :session-id :model :cwd :preview.
:preview is the first line of the first user prompt, if present."
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
            :started started
            :session-id session-id
            :model model
            :cwd cwd
            :preview preview))))

(defun agent-shell-sidebar--cached-header (file)
  "Return the cached header plist for FILE if fresh, else nil.
An entry is considered fresh when its schema matches
`agent-shell-sidebar--cache-schema' and its recorded mtime matches
FILE's current mtime."
  (when-let* ((entry (gethash file agent-shell-sidebar--parse-cache))
              (schema (plist-get entry :schema))
              ((equal schema agent-shell-sidebar--cache-schema))
              (mtime (plist-get entry :mtime))
              (attrs (file-attributes file))
              (current-mtime (and attrs
                                  (float-time
                                   (file-attribute-modification-time attrs)))))
    (when (equal mtime current-mtime)
      (plist-get entry :header))))

(defun agent-shell-sidebar--ensure-parsed (file)
  "Return header for FILE from cache, parsing synchronously if missing."
  (or (agent-shell-sidebar--cached-header file)
      (let* ((attrs (file-attributes file))
             (mtime (and attrs
                         (float-time
                          (file-attribute-modification-time attrs))))
             (header (agent-shell-sidebar--parse-header-from-file file)))
        (puthash file (list :schema agent-shell-sidebar--cache-schema
                            :mtime mtime
                            :header header)
                 agent-shell-sidebar--parse-cache)
        header)))

;;; Async parse loop

(defun agent-shell-sidebar--queue-uncached (paths)
  "Push any of PATHS that lack a fresh cache entry onto the parse queue."
  (dolist (p paths)
    (unless (agent-shell-sidebar--cached-header p)
      (unless (member p agent-shell-sidebar--parse-queue)
        (setq agent-shell-sidebar--parse-queue
              (nconc agent-shell-sidebar--parse-queue (list p)))))))

(defun agent-shell-sidebar--start-parse-timer ()
  "Kick off the idle timer if there is work to do and it's not running."
  (when (and agent-shell-sidebar--parse-queue
             (not (timerp agent-shell-sidebar--parse-timer)))
    (setq agent-shell-sidebar--parse-timer
          (run-with-idle-timer
           agent-shell-sidebar-parse-idle-delay t
           #'agent-shell-sidebar--parse-tick))))

(defun agent-shell-sidebar--parse-tick ()
  "Parse the next chunk of transcripts, then redraw affected sidebars."
  (let ((n 0)
        (parsed nil))
    (while (and agent-shell-sidebar--parse-queue
                (< n agent-shell-sidebar-parse-chunk-size))
      (let ((file (pop agent-shell-sidebar--parse-queue)))
        (when (file-exists-p file)
          (agent-shell-sidebar--ensure-parsed file)
          (push file parsed)))
      (cl-incf n))
    (when parsed
      (dolist (buf (buffer-list))
        (when (and (buffer-live-p buf)
                   (eq (buffer-local-value 'major-mode buf)
                       'agent-shell-sidebar-mode))
          (with-current-buffer buf
            (agent-shell-sidebar--redraw)))))
    (unless agent-shell-sidebar--parse-queue
      (when (timerp agent-shell-sidebar--parse-timer)
        (cancel-timer agent-shell-sidebar--parse-timer))
      (setq agent-shell-sidebar--parse-timer nil))))

;;; Live-buffer + config lookups

(defun agent-shell-sidebar--live-buffer-for-session (session-id)
  "Return a live agent-shell buffer whose session id equals SESSION-ID."
  (and session-id
       (seq-find (lambda (buf)
                   (with-current-buffer buf
                     (equal session-id
                            (map-nested-elt agent-shell--state
                                            '(:session :id)))))
                 (agent-shell-buffers))))

(defun agent-shell-sidebar--config-for-agent-name (name)
  "Return the agent config whose display name matches NAME, or nil.
First tries an exact match on `:mode-line-name' or `:buffer-name',
then falls back to a case-insensitive substring match either way."
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

;;; Rendering

(defconst agent-shell-sidebar--placeholder
  (propertize "…" 'face 'agent-shell-sidebar-placeholder-face))

(defun agent-shell-sidebar--short-date (started file)
  "Return a short MM-DD HH:MM date string.
STARTED is the header value; falls back to FILE's mtime."
  (or (and started
           (string-match "^\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)[ T]\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
                         started)
           (format "%s-%s %s:%s"
                   (match-string 2 started)
                   (match-string 3 started)
                   (match-string 4 started)
                   (match-string 5 started)))
      (and file (file-exists-p file)
           (format-time-string
            "%m-%d %H:%M"
            (file-attribute-modification-time (file-attributes file))))
      "??-?? ??:??"))

(defun agent-shell-sidebar--short-agent (name)
  "Return a short (<=6ch) agent name for display."
  (if (and name (> (length name) 8))
      (substring name 0 8)
    (or name "?")))

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
  "Group PATHS into an alist ((AGENT . (PATH ...)) ...).
Within each group PATHS are ordered as passed in (mtime-desc from
`--collect').  Groups are then sorted by newest transcript overall.
AGENT is the string from the transcript header, or nil when unknown."
  (let ((groups (make-hash-table :test 'equal))
        (keys nil))
    (dolist (p paths)
      (let* ((header (agent-shell-sidebar--cached-header p))
             (agent (and header (plist-get header :agent))))
        (unless (gethash agent groups)
          (push agent keys))
        (push p (gethash agent groups))))
    (let ((entries (mapcar (lambda (a)
                             (cons a (nreverse (gethash a groups))))
                           (nreverse keys))))
      (sort entries
            (lambda (a b)
              (let ((ma (and (cdr a)
                             (file-attribute-modification-time
                              (file-attributes (car (cdr a))))))
                    (mb (and (cdr b)
                             (file-attribute-modification-time
                              (file-attributes (car (cdr b)))))))
                (cond
                 ((and (null ma) (null mb))
                  (string< (or (car a) "") (or (car b) "")))
                 ((null ma) nil)
                 ((null mb) t)
                 (t (time-less-p mb ma)))))))))

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
         (mark-str (if (eq mark 'delete)
                       (propertize "D" 'face 'agent-shell-sidebar-mark-face)
                     " "))
         (start (point)))
    (insert "    " mark-str " " date
            (cond
             (preview (concat ": " preview))
             (parsed "")
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

(defun agent-shell-sidebar--redraw ()
  "Redraw the current sidebar buffer, preserving point on the same file/project."
  (when (derived-mode-p 'agent-shell-sidebar-mode)
    (let* ((prev-file (get-text-property (point) 'agent-shell-sidebar-file))
           (prev-agent (get-text-property (point) 'agent-shell-sidebar-agent))
           (prev-project (get-text-property (point) 'agent-shell-sidebar-project))
           (prev-line (line-number-at-pos))
           (inhibit-read-only t)
           (groups (agent-shell-sidebar--collect)))
      (erase-buffer)
      (unless agent-shell-sidebar--marks
        (setq agent-shell-sidebar--marks (make-hash-table :test 'equal)))
      (unless agent-shell-sidebar--collapsed
        (setq agent-shell-sidebar--collapsed (make-hash-table :test 'equal)))
      (if (null groups)
          (insert (propertize "  (no transcripts found)\n"
                              'face 'agent-shell-sidebar-placeholder-face))
        (let ((names (agent-shell-sidebar--disambiguated-names
                      (mapcar #'car groups))))
          (dolist (group groups)
            (let* ((root (car group))
                   (paths (cdr group))
                   (project-collapsed
                    (gethash root agent-shell-sidebar--collapsed))
                   (name (or (gethash root names)
                             (file-name-nondirectory
                              (directory-file-name root)))))
              (agent-shell-sidebar--render-project-header
               root (length paths) project-collapsed name)
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
                         p (gethash p agent-shell-sidebar--marks)))))))))))
      (goto-char (point-min))
      (cond
       (prev-file
        (let ((found nil))
          (while (and (not found) (not (eobp)))
            (when (equal prev-file
                         (get-text-property (point) 'agent-shell-sidebar-file))
              (setq found t))
            (unless found (forward-line 1)))
          (unless found (goto-char (point-min))
                  (forward-line (1- prev-line)))))
       (prev-agent
        (let ((found nil))
          (while (and (not found) (not (eobp)))
            (when (equal prev-agent
                         (get-text-property (point) 'agent-shell-sidebar-agent))
              (setq found t))
            (unless found (forward-line 1)))
          (unless found (goto-char (point-min)))))
       (prev-project
        (let ((found nil))
          (while (and (not found) (not (eobp)))
            (when (equal prev-project
                         (get-text-property (point) 'agent-shell-sidebar-project))
              (setq found t))
            (unless found (forward-line 1)))
          (unless found (goto-char (point-min)))))
       (t (forward-line (1- prev-line)))))))

;;; Mode + keymap

(defvar agent-shell-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agent-shell-sidebar-visit)
    (define-key map (kbd "o") #'agent-shell-sidebar-open-transcript)
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
  "Major mode for the agent-shell chat sidebar."
  :group 'agent-shell-sidebar
  (setq truncate-lines t
        buffer-read-only t
        window-size-fixed agent-shell-sidebar-window-fixed
        cursor-in-non-selected-windows nil)
  (setq-local agent-shell-sidebar--marks (make-hash-table :test 'equal))
  (setq-local agent-shell-sidebar--collapsed (make-hash-table :test 'equal))
  (when agent-shell-sidebar-refresh-timer
    (setq-local agent-shell-sidebar--refresh-timer-object
                (run-with-idle-timer
                 agent-shell-sidebar-refresh-timer t
                 (lambda ()
                   (when (buffer-live-p (get-buffer agent-shell-sidebar-name))
                     (with-current-buffer (get-buffer agent-shell-sidebar-name)
                       (agent-shell-sidebar-refresh)))))))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when (timerp agent-shell-sidebar--refresh-timer-object)
                (cancel-timer agent-shell-sidebar--refresh-timer-object)))
            nil t))

;;; Commands: navigation

(defun agent-shell-sidebar-next-line (&optional n)
  "Move to the next actionable row (file, agent, or project)."
  (interactive "p")
  (let ((n (or n 1)))
    (dotimes (_ (abs n))
      (forward-line (if (> n 0) 1 -1))
      (while (and (not (eobp))
                  (not (bobp))
                  (not (get-text-property (point) 'agent-shell-sidebar-file))
                  (not (get-text-property (point) 'agent-shell-sidebar-agent))
                  (not (get-text-property (point) 'agent-shell-sidebar-project)))
        (forward-line (if (> n 0) 1 -1))))))

(defun agent-shell-sidebar-previous-line (&optional n)
  "Move to the previous transcript row (skipping project headers)."
  (interactive "p")
  (agent-shell-sidebar-next-line (- (or n 1))))

;;; Commands: activation

(defun agent-shell-sidebar--pop-to (buffer)
  "Pop to BUFFER in the MRU or next window, per user setting."
  (let ((win (if agent-shell-sidebar-open-file-in-most-recently-used-window
                 (get-mru-window nil nil t)
               (next-window))))
    (if win
        (progn (select-window win)
               (switch-to-buffer buffer))
      (pop-to-buffer buffer))))

(defun agent-shell-sidebar-visit ()
  "Activate the row at point.
On a project or agent header, toggle the fold.  On a transcript row,
start an agent-shell that resumes the transcript's session (or reuses
an already-live shell for that session)."
  (interactive)
  (cond
   ((or (get-text-property (point) 'agent-shell-sidebar-project)
        (get-text-property (point) 'agent-shell-sidebar-agent))
    (agent-shell-sidebar-toggle-project))
   ((get-text-property (point) 'agent-shell-sidebar-file)
    (let* ((file (get-text-property (point) 'agent-shell-sidebar-file))
           (header (agent-shell-sidebar--ensure-parsed file))
           (session-id (plist-get header :session-id))
           (agent-name (plist-get header :agent))
           (cwd (plist-get header :cwd))
           (live (agent-shell-sidebar--live-buffer-for-session session-id)))
      (cond
       (live
        (agent-shell-sidebar--pop-to live))
       (t
        (let* ((default-directory
                (if (and cwd (file-directory-p cwd))
                    (file-name-as-directory cwd)
                  default-directory))
               (config (or (agent-shell-sidebar--config-for-agent-name
                            agent-name)
                           (and (fboundp 'agent-shell--auto-preferred-config)
                                (agent-shell--auto-preferred-config))
                           (agent-shell-select-config
                            :prompt "Start with agent: ")))
               (buf (agent-shell--start
                     :config config
                     :session-id session-id
                     :session-strategy 'new
                     :new-session t
                     :no-focus t)))
          (agent-shell-sidebar--pop-to buf))))))
   (t (user-error "No transcript at point"))))

(defun agent-shell-sidebar-mouse-visit (event)
  "Handle mouse click EVENT on a row."
  (interactive "e")
  (let ((posn (event-end event)))
    (with-current-buffer (window-buffer (posn-window posn))
      (goto-char (posn-point posn))
      (agent-shell-sidebar-visit))))

(defun agent-shell-sidebar-open-transcript ()
  "Open the transcript markdown at point in view mode."
  (interactive)
  (let ((file (get-text-property (point) 'agent-shell-sidebar-file)))
    (unless file (user-error "No transcript at point"))
    (agent-shell-sidebar--pop-to (find-file-noselect file))))

(defun agent-shell-sidebar-toggle-project ()
  "Fold or unfold the project or agent section at point."
  (interactive)
  (let ((key (or (get-text-property (point) 'agent-shell-sidebar-project)
                 (get-text-property (point) 'agent-shell-sidebar-agent))))
    (unless key (user-error "Not on a project or agent header"))
    (if (gethash key agent-shell-sidebar--collapsed)
        (remhash key agent-shell-sidebar--collapsed)
      (puthash key t agent-shell-sidebar--collapsed))
    (agent-shell-sidebar--redraw)))

;;; Commands: marks

(defun agent-shell-sidebar-mark-delete ()
  "Mark the transcript at point for deletion."
  (interactive)
  (let ((file (get-text-property (point) 'agent-shell-sidebar-file)))
    (unless file (user-error "No transcript at point"))
    (puthash file 'delete agent-shell-sidebar--marks)
    (agent-shell-sidebar--redraw)
    (agent-shell-sidebar-next-line 1)))

(defun agent-shell-sidebar-unmark ()
  "Remove any mark from the transcript at point."
  (interactive)
  (let ((file (get-text-property (point) 'agent-shell-sidebar-file)))
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
  "Delete all transcripts marked for deletion."
  (interactive)
  (let (files)
    (maphash (lambda (path mark)
               (when (eq mark 'delete) (push path files)))
             agent-shell-sidebar--marks)
    (cond
     ((null files) (message "No marks to execute"))
     ((yes-or-no-p (format "Delete %d transcript file(s)? " (length files)))
      (dolist (f files)
        (when (file-exists-p f)
          (delete-file f))
        (remhash f agent-shell-sidebar--parse-cache)
        (remhash f agent-shell-sidebar--marks))
      (agent-shell-sidebar-refresh)
      (message "Deleted %d file(s)" (length files))))))

;;; Commands: refresh

(defun agent-shell-sidebar-refresh ()
  "Rescan transcripts and redraw."
  (interactive)
  (let ((all-paths
         (apply #'append
                (mapcar #'cdr (agent-shell-sidebar--collect)))))
    (agent-shell-sidebar--queue-uncached all-paths)
    (agent-shell-sidebar--redraw)
    (agent-shell-sidebar--start-parse-timer)))

;;; Sidebar window commands

(defun agent-shell-sidebar--sidebar-buffer (&optional _frame)
  "Return the live sidebar buffer if any is displayed in the current frame."
  (seq-find
   (lambda (w)
     (with-current-buffer (window-buffer w)
       (derived-mode-p 'agent-shell-sidebar-mode)))
   (window-list)))

(defun agent-shell-sidebar-showing-sidebar-p ()
  "Non-nil if the sidebar is visible in the selected frame."
  (agent-shell-sidebar--sidebar-buffer))

(defun agent-shell-sidebar--get-or-create-buffer ()
  "Return the sidebar buffer, creating and initializing it if needed."
  (let ((existing (get-buffer agent-shell-sidebar-name)))
    (or existing
        (let ((buf (generate-new-buffer agent-shell-sidebar-name)))
          (with-current-buffer buf
            (agent-shell-sidebar-mode)
            (agent-shell-sidebar-refresh))
          buf))))

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
  "Show the agent-shell chat sidebar."
  (interactive)
  (let ((buffer (agent-shell-sidebar--get-or-create-buffer)))
    (display-buffer-in-side-window buffer agent-shell-sidebar-display-alist)
    (let ((window (get-buffer-window buffer)))
      (when window
        (set-window-dedicated-p window t)
        (when agent-shell-sidebar-no-delete-other-windows
          (set-window-parameter window 'no-delete-other-windows t))
        (when agent-shell-sidebar-resize-on-open
          (with-selected-window window
            (let ((window-size-fixed))
              (agent-shell-sidebar--set-width agent-shell-sidebar-width))))))))

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
  "Jump to the sidebar window, showing it first if hidden."
  (interactive)
  (if-let* ((win (agent-shell-sidebar--sidebar-buffer)))
      (select-window win)
    (call-interactively #'agent-shell-sidebar-toggle-sidebar)))

;;; Evil bindings

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal agent-shell-sidebar-mode-map
      (kbd "RET") #'agent-shell-sidebar-visit
      (kbd "o")   #'agent-shell-sidebar-open-transcript
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
