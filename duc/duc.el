;;;; -*- lexical-binding: t; -*-

(defmacro duc/alist-replace (list-var element)
  `(let
       ((replaced-list-var
         (assq-delete-all
          (car ',element) ,list-var)))
     (setq ,list-var
           (add-to-list 'replaced-list-var ',element))))

(defmacro duc/alist-replace-set (list-var element)
  `(setq ,list-var (duc/alist-replace ,list-var ,element)))

(defvar duc/font-family "Iosevka")
(defvar duc/font-height 160)

(defvar duc/font-family-mode-line "Concourse T3 Tab")
(defvar duc/font-height-mode-line 160)

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
                      :family duc/font-family
                      :height duc/font-height
                      :weight duc/font-weight
                      :width 'normal))

(defun duc/set-font-line-spacing ()
  (interactive)
  (setq duc/font-line-spacing (string-to-number (completing-read "line spacing: "
                                                                 '("0"))))
  (setq-default line-spacing duc/font-line-spacing))

(defun duc/selectrum-load-theme ()
  (interactive)
  (load-theme (intern
               (selectrum-read "Load custom theme: "
                               (mapcar 'symbol-name
                                       (custom-available-themes))
                               :require-match t))
              t))

(defun duc/ivy-shell ()
  (interactive)
  (let ((terminal-buffers (seq-filter (lambda (x)
                                        (string-match-p
                                         (regexp-quote "terminal-") x))
                                      (mapcar (function buffer-name) (buffer-list)))))
    (let ((buffer-name (completing-read "shell : " terminal-buffers)))
     (if (member buffer-name terminal-buffers)
         (switch-to-buffer buffer-name)
       (vterm (concat "terminal-" buffer-name))))))

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



;; e.g. [[file:~/tb/tableau-auth-android/tableauauth/src/main/java/com/tableau/tableauauth/webauth/WebAuthActivity.kt::250]]
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

(defun duc/eval-dwim (p)
  (interactive "P")
  (pcase major-mode
    ('racket-mode (duc/racket-eval-last-sexp))
    ('emacs-lisp-mode (eval-last-sexp p))
    ('python-mode
     (unless (get-buffer (format "*Python[%s]*" (buffer-name)))
       (let ((buffer (buffer-name)))
         (run-python nil t t)
         (pop-to-buffer buffer)))
     (cond ((use-region-p) (python-shell-send-string
                            (buffer-substring (region-beginning)
                                              (region-end))))
           (t (python-shell-send-buffer))))
    ('latex-mode (preview-section))
    (_ (eval-last-sexp p))))

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

(defun duc/eval-last (p)
  (interactive "P")
  (pcase major-mode
    ('emacs-lisp-mode (eval-last-sexp p))
    (_ (eval-last-sexp p))))

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
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (switch-to-buffer-other-window buffer)))

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

(defun duc/create-or-open-bnote ()
  (interactive)
  (let* ((new-entry (concat duc/create-bnote-default-dir
                           "bnote"
                           "-"
                           (format-time-string "%y%2m%2d")
                           ".org"
                           ))
         (new-entry-exists-p (or (get-buffer new-entry)
                                 (file-exists-p new-entry))))
    (unless new-entry-exists-p
      ; Entry not found, so we'll create a new entry.
      (let ((last-entry (car (last (directory-files duc/create-bnote-default-dir
                                                    t
                                                    "^bnote-")))))
        (if (and last-entry (file-exists-p last-entry))
            (copy-file last-entry
                       new-entry)
          (append-to-file "#+STARTUP: showeverything
#+LINK: ◊ file:bnote-%s.org

* Daily Log

* Reference
  • *Tasks*: things you have to do. s-T.
x • Task complete.
  - *Notes*: Things you don't want to forgot. s-N.
  ◦ *Events*: Noteworth moments in time. s-E.
 +• Task irrelevant.+
^ - Priority note.
! - Inspiration note.
  - [[◊:210101]]. To refer to other notes. s-L.
  - Format is adapted from [[https://bulletjournal.com]]."
                          nil new-entry))))
    (find-file new-entry)
    (unless new-entry-exists-p
      (search-forward "* Daily Log"))))

(defun duc/insert-bnote-lozenge-empty-link ()
    (interactive)
  (insert (concat "[[◊:" (format-time-string "%y%2m%2d") "]]")))

(defun duc/add-bnote-with-char (bullet)
  (interactive)
  (let ((indent-level
         (or (string-match-p "[•◦<>-]"
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
  (let ((fc (current-fill-column)))
    (when (and fc (> (current-column) fc))
      (let* ((fill-prefix
              (replace-regexp-in-string "[•◦<>-]"
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

(defun duc/anki-connect-push ()
  (interactive)
  (let ()
    ;; enable anki-editor-mode if it isn't. Enabling the mode will enable uploading local media to anki.
    (if (not (bound-and-true-p anki-editor-mode))
        (anki-editor-mode))
    (anki-editor-push-notes)))

(defun duc/play-sound (&optional filepath)
  (interactive)
  (let ((filepath (shell-quote-argument ; Escape special characters to be
                                        ; compatible with words like
                                        ; 'Raison d’être'.
                   (expand-file-name    ; Resolve path so that tilde char
                                        ; won't be escaped and make the
                                        ; path invalid.
                    (or filepath (completing-read "sound file to play: "))))))
    (shell-command-to-string (format "afplay %s" filepath))))

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
  (let ((apikey local/forvo-api-key)
        (request-backend 'curl)
        (json-array-type 'list)
        reply
        err)
                               ; e.g. https://apifree.forvo.com/action/word-pronunciations/format/json/word/forvo/id_lang_speak/39/order/rate-desc/limit/1/key/XXXX/
    (let ((response (request (format "https://apifree.forvo.com/action/word-pronunciations/format/json/word/%s/id_lang_speak/39/order/rate-desc/limit/1/key/%s/"
                                     word
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

(provide 'duc)
