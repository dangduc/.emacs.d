;;;; -*- lexical-binding: t; -*-
;; package management
;;


; Alist of major and minor modes to leader-g transient.
; leader-g transients are mutually-exclusive. Minor mode transients
; have priority over major mode transients.
(defvar g-mode-alist '())

(defun g-mode-to-transient ()
  (interactive)
  (let ((transient-fn (cdr (or (assoc (seq-find (lambda (minor-mode)
                                                  (assoc minor-mode g-mode-alist))
                                                local-minor-modes)
                                      g-mode-alist)
                               (assoc major-mode
                                      g-mode-alist)))))
    (if transient-fn
        (apply transient-fn nil)
      (message "No leader-g transient"))))

(use-package bind-key
  :ensure nil) ;; built-in (ships with use-package)

(use-package transient
  :config
  ;; Bind esc
  (define-key transient-map (kbd "<escape>") 'transient-quit-all)
  (define-key transient-edit-map (kbd "<escape>") 'transient-quit-one)
  (define-key transient-sticky-map (kbd "<escape>") 'transient-quit-seq)
  (define-key transient-map (kbd "q") 'transient-quit-all)
  (define-key transient-edit-map (kbd "q") 'transient-quit-one)
  (define-key transient-sticky-map (kbd "q") 'transient-quit-seq)

  (transient-define-prefix transient-buffer ()
    "buffer"
    ["Switches"
     ("w" "other-window-prefix" "--other-window-prefix")]
    [["edit"
      ("N" "new" duc/new-buffer)
      ("m" "move buffer & file (ie, rename)" (lambda () (interactive) (duc/rename-file (buffer-name))))
      ("r" "rename" rename-buffer)
      ("R" "reload" revert-buffer)
      ("k" "kill buffer" kill-buffer)]
     ["navigation"
      ("p" "prev" previous-buffer)
      ("n" "next" next-buffer)
      ("l" "list buffers" list-buffers)
      ("o" "switch" switch-to-buffer)]
     ["other"
      ("i" "create indirect buffer" clone-indirect-buffer)
      ("t" "tail -f" auto-revert-tail-mode)
      ("y" "yank buffer name" duc/yank-buffer-name)]])
  (transient-define-prefix transient-window ()
    "window"
    [["Frame"
      ("w" "toggle maximize" toggle-frame-maximized)
      ("n" "next" other-frame)
      ("N" "new" make-frame-command)]
     ["Window"
      ("b" "balance" balance-windows)
      ("k" "kill" delete-window)
      ("p" "toggle pin" duc/toggle-pin-buffer)]
     ["Other"
      ("o" "next buffer command in OTHER window" (lambda ()
                                                   (interactive) (other-window-prefix)))
      ("O" "next buffer command in SAME window" (lambda ()
                                                  (interactive) (same-window-prefix)))]])
  (transient-define-prefix transient-file ()
    "file"
    [["navigate"
      ("f" "find file" find-file)
      ("i" "package-declarations.el" (lambda () (interactive) (find-file "~/.emacs.d/lisp/package-declarations.el")))
      ("I" "init.el" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
      ("1" "index.org" (lambda () (interactive) (find-file "~/dev/notes/index.org")))
      ("b" "sidebar" duc/sidebar-toggle)]
     ["edit"
      ("w" "write file" save-buffer)
      ("K" "delete file" duc/delete-this-file)]
     ["note"
      ("l" "bnote" (lambda () (interactive) (duc/create-or-open-bnote-type "bnote")))
      ("d" "agenda" (lambda () (interactive) (duc/create-or-open-bnote-type "daily-agenda")))
      ("m" "morning" (lambda () (interactive) (duc/create-or-open-bnote-type "morning")))
      ("e" "evening" (lambda () (interactive) (duc/create-or-open-bnote-type "evening")))
      ("L" "create w/ type" duc/completing-bnote-type)
      ("c" "capture note" (lambda () (interactive) (org-capture nil "c")))
      ("C" "capture longer note" (lambda () (interactive) (org-capture nil "C")))
      ("t" "capture todo" (lambda () (interactive) (org-capture nil "t")))
      ("r" "capture region" duc/org-capture-region-with-code-block)]
     ["other"
      ("y" "yank filename (relative to project)" duc/yank-file-path-relative-to-project)
      ("Y" "yank parent directory of file" duc/yank-absolute-path-to-parent)
      ;; e.g. "nc termbin.com 9999"
      ("3" "M-|" shell-command-on-region)]])
  (transient-define-prefix transient-major-pdf-view ()
    "pdf-view-mode"
    [["pdf-view-mode"
      ("l" "yank link" duc/yank-org-pdftools-get-link)]])
  (defun transient-major ()
    (interactive)
    (pcase major-mode
      ('pdf-view-mode (transient-major-pdf-view))
      (_ (g-mode-to-transient))))
  (transient-define-prefix transient-org-roam ()
    "org-roam"
    [["node"
      ("i" "insert" org-roam-node-insert)
      ("N" "insert" org-roam-node-insert)
      ("f" "find" org-roam-node-find)
      ("c" "capture" org-roam-capture)
      ("s" "toggle buffer" org-roam-buffer-toggle)]
     ["dailies"
      ("l" "daily today" org-roam-dailies-goto-today)
      ("L" "daily previous" org-roam-dailies-goto-previous-note)]
     ["ui"
      ("u" "ui" org-roam-ui-mode)
      ("z" "local" org-roam-ui-node-local)
      ("Z" "zoom" org-roam-ui-node-zoom)]])
  (transient-define-prefix transient-org-mode ()
    "org-mode"
    [["edit"
      ("c" "C-c C-c" org-ctrl-c-ctrl-c)
      ("m" "region->md" org-md-convert-region-to-md)
      ("t" "insert template" org-insert-structure-template)
      ("I" "take screenshot" org-download-screenshot)
      ("e" "encrypt entry" org-encrypt-entry)
      ("E" "encrypt all" org-encrypt-entries)
      ("d" "decrypt entry" org-decrypt-entry)
      ("D" "decrypt all" org-decrypt-entries)]
     ["bnote"
      ("b" "bnote" (lambda () (interactive) (duc/create-or-open-bnote-type "bnote")))
      ("l" "search & insert link" duc/completing-bnote-insert-linked-link)
      ("s" "store link at P" org-store-link)
      ("S" "insert link at P" org-insert-link)]
     ["view"
      ("A" "agenda" org-agenda)
      ("L" "toggle descriptive links" org-toggle-link-display)
      ("i" "toggle inline images" org-toggle-inline-images)
      ("n" "narrow" org-narrow-to-subtree)
      ("N" "widen" widen)
      ("v" "toggle word-wrap" (lambda () (interactive)
                                (visual-line-mode 'toggle)))
      ("o" "open link" (lambda () (interactive)
                          (org-open-at-point)
                          (balance-windows)))
      ("w" "log work entry" org-worklog-entry)]])
  (transient-define-prefix transient-org-fc ()
    "org-fc"
    [["Capture"
      ("N" "normal" (lambda () (interactive) (org-capture nil "n")))
      ("n" "normal" org-fc-type-normal-init)
      ("c" "cloze" (lambda () (interactive) (org-fc-type-cloze-init 'deletion)))]
     ["Other"
      ("r" "review buffer" org-fc-review-buffer)
      ("R" "review all" org-fc-review-all)
      ("m" "dashboard" org-fc-dashboard)
      ("S" "screenshot" (lambda () (interactive)
                          (let ((org-download-image-dir "~/dev/org-fc/img"))
                            (org-download-screenshot))))
      ("h" "hydra" org-fc-hydra/body)]]))

(use-package duc
  :ensure nil ;; local package in lisp/
  :init
                                        ; e.g., switch-to-buffer respects other-window-prefix
  (setq switch-to-buffer-obey-display-actions t)

  :config
  ;; Set font. `find-font' returns nil when there is no display (e.g. --batch
  ;; or daemon startup) or when the font isn't installed; passing :font/:family
  ;; nil to `set-face-attribute' signals "Invalid font or font-spec" and aborts
  ;; the rest of this :config block. Guard on the lookup so startup is robust.
  (when (find-font (font-spec :name duc/font-family))
    (set-face-attribute 'default nil
                        :font (find-font (font-spec :name duc/font-family))
                        :height duc/font-height
                        :weight duc/font-weight
                        :width 'unspecified))

  (when (find-font (font-spec :name duc/font-family-variable-pitch))
    (set-face-attribute 'variable-pitch nil
                        :family (find-font (font-spec :name duc/font-family-variable-pitch))
                        :height 'unspecified
                        :weight 'unspecified
                        :width 'unspecified))

  (with-eval-after-load 'org-faces
    (set-face-attribute 'org-block nil
                        :inherit '(fixed-pitch shadow)
                        :extend t
                        :family duc/font-family))

  (defun duc/theme-setup-mode-line-font (&rest _)
    (dolist (sym '(mode-line mode-line-inactive))
      (set-face-attribute sym nil
                          :family duc/font-family-mode-line
                          :height duc/font-height-mode-line
                          :weight duc/font-weight
                          :width 'normal)))

  (duc/theme-setup-mode-line-font)

  (advice-add 'load-theme :after #'duc/theme-setup-mode-line-font)

  (let ((enable-mode-line-setup nil))
    (when enable-mode-line-setup
      (duc/theme-setup-mode-line)
      (advice-add 'load-theme :after #'duc/theme-setup-mode-line)))

  (add-hook 'org-mode-hook
            (lambda ()
              (progn
                (visual-line-mode 1)
                (company-mode -1))))

  (add-hook 'before-save-hook
            (lambda ()
              (pcase major-mode
                ('org-mode (if (s-ends-with-p "/dev/notes/" (file-name-directory (buffer-file-name)))
                               (duc/bnote-update-backlinks-for-note))))))

  (duc/prd-dices-file-load))

(with-eval-after-load 'evil
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-inactive-mode-map [escape] 'minibuffer-keyboard-quit))

(with-eval-after-load 'calc
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((calc . t))))

(require 'pulsar)
(with-eval-after-load 'pulsar
  (setq pulsar-pulse-functions '(duc/eval-dwim
                                 xref-find-definitions
                                 xref-find-references
                                 tide-jump-to-filespan
                                 tide-references))
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))

(global-unset-key (kbd "s-q"))

(use-package diminish
  :config
  (diminish 'subword-mode)
  (diminish 'visual-line-mode)
  (diminish 'abbrev-mode)
  (with-eval-after-load 'eldoc
    (diminish 'eldoc-mode))
  (with-eval-after-load 'hideshow
    (diminish 'hs-minor-mode))
  (with-eval-after-load 'autorevert
    (diminish 'auto-revert-mode)))

(use-package tab-bar
  :ensure nil
  :config
  ;; Make tab bar switches create a new tab if there were no tabs to switch to.
  (advice-add 'tab-bar-switch-to-next-tab
              :after
              (lambda (&rest _)
                (when (= (length (funcall tab-bar-tabs-function)) 1)
                  (tab-new 1))))
  (advice-add 'tab-bar-switch-to-prev-tab
              :after
              (lambda (&rest _)
                (when (= (length (funcall tab-bar-tabs-function)) 1)
                  (tab-new -1))))

  ;; Show tab bar only when we have more than 1 tab.
  (setf tab-bar-show 1))

(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.2)
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package async
  :init
  (add-hook 'dired-mode-hook
            (lambda ()
              (when (file-remote-p default-directory)
                (dired-async-mode)))))

(use-package exec-path-from-shell
  :after async
  :if (memq window-system '(mac ns x))
  :init
  ;; Set the shell environment properly.
  (defun exec-path-from-shell-copy-envs-async (names)
    "Run `exec-path-from-shell-copy-envs' asynchronously."
    (async-start
     `(lambda ()
        (load ,(locate-library "exec-path-from-shell"))
        (require 'exec-path-from-shell)
        (exec-path-from-shell-getenvs ',names))
     (lambda (pairs)
       (when pairs
         (require 'exec-path-from-shell)
         (mapc (lambda (pair)
                 (exec-path-from-shell-setenv (car pair) (cdr pair)))
               pairs)))))
  (exec-path-from-shell-copy-envs-async '("PATH")))

(use-package evil
  :init
  ; next two lines required for evil-collection.
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  ; hideshow
  (add-hook 'json-mode-hook 'hs-minor-mode)
  (add-hook 'java-mode-hook 'hs-minor-mode)
  (add-hook 'java-ts-mode-hook 'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'tide-mode-hook 'hs-minor-mode)
  (add-hook 'typescript-mode-hook 'hs-minor-mode)
  (add-hook 'typescript-ts-base-mode-hook 'hs-minor-mode)
  (add-hook 'kotlin-mode-hook 'hs-minor-mode)
  (add-hook 'swift-mode-hook 'hs-minor-mode)
  ; `js-base-mode' covers both `js-mode' and `js-ts-mode'.
  (add-hook 'js-base-mode-hook 'hs-minor-mode)
  :config
  ; This _somehow_ fixes emacs deterministically freezing while (`/`) searching
  ; for certain strings.
  ; https://github.com/syl20bnr/spacemacs/issues/3623
  (setq-default search-invisible t)
  ;
  (setq evil-want-C-u-scroll t)
  ; hideshow
  (evil-define-key 'normal hs-minor-mode-map (kbd "<tab>") 'hs-toggle-hiding)
  (evil-define-key 'normal hs-minor-mode-map (kbd "<S-tab>") 'hs-hide-all)
  (evil-define-key 'normal hs-minor-mode-map (kbd "<backtab>") 'hs-hide-all)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  ; " make j, k move cursor screen-wise, not line-wise. Makes word-wrapped
  ; " paragraph navigation sane. (http://statico.github.com/vim.html)
  ; :nmap j gj
  ; :nmap k gk
  ;                                      ;
  ; [How to map j and k to gj and gk in Emacs Evil-mode?](https://stackoverflow.com/questions/23576163/)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
  ; " scroll by N lines instead of default 1 line.
  ; set scrolloff=3
  (setq scroll-margin 3)
  (evil-mode 1))

(use-package evil-collection
    :after evil
    :config
    ;; org-mode
    (evil-define-key 'emacs org-agenda-mode-map (kbd "j") 'evil-next-line)
    (evil-define-key 'emacs org-agenda-mode-map (kbd "k") 'evil-previous-line)
    (evil-define-key '(normal insert) org-mode-map (kbd "C-c i") 'org-clock-in)
    (evil-define-key '(normal insert) org-mode-map (kbd "C-c o") 'org-clock-out)
    ; Binding not working in WSL. Not sure why.
    (evil-define-key '(normal) org-mode-map (kbd "<tab>") 'org-cycle)
    (evil-collection-init))

(use-package general
  :after transient
  :config
  (general-override-mode)
  (general-define-key
   :states '(normal motion visual emacs)
   :keymaps 'override
   "<SPC>" 'leader-main-menu)

  (transient-define-infix leader-main-menu-flag-use-cache ()
    :description "Invalidate cache (for some actions)"
    :class 'transient-switch
    :key "`c"
    :argument "--invalidate-cache")

  (transient-define-prefix leader-main-menu ()
    "Main"
    ["Args" (leader-main-menu-flag-use-cache)]
    [["Navigate"
      ("h" "left" evil-window-left)
      ("l" "right" evil-window-right)
      ("k" "up" evil-window-up)
      ("j" "down" evil-window-down)
      ("-" "vsplit" split-window-below)
      ("\\" "hsplit" split-window-right)
      ("s" "split" (lambda () (interactive) (split-window) (balance-windows)))
      ("a" "ace" ace-window)]
     ["Search"
      ("," "in files" consult-ripgrep)
      ("<" "occur in files" deadgrep)
      ("B" "occur in file " occur)]
     ["Action"
      ("SPC" "M-x" execute-extended-command)
      ("b" "buffers" transient-buffer)
      ("e" "eval" hydra-submenu-eval/body)
      ("w" "window/frame" transient-window)
      ("L" "lc" hydra-submenu-leetcode/body)]
     ["Application"
      ("g" "major" transient-major)
      ("v" "magit" hydra-submenu-git/body)
      ("o" "org-mode" transient-org-mode)
      ("r" "org-roam" transient-org-roam)
      ("E" "eval-expresssion (M-:)" eval-expression)
      (":" "eval-expresssion (M-:)" eval-expression)
      ("t" "terminal" duc/ivy-terminal)
      ("T" "send to terminal" duc/shell-send-string-to-project-dwim)
      ("C" "claude" hydra-submenu-claude/body)
      ("i" "agent-shell" hydra-submenu-agent-shell/body)
      ("u" "package" hydra-submenu-package/body)
      ("A" "anki" hydra-submenu-anki/body)
      ("R" "org-fc" transient-org-fc)
      ("d" "rpgdm" hydra-rpgdm/body
       :if (lambda () (fboundp 'hydra-rpgdm/body)))
      ]]
    [["More Navigation"
      ("n" "buffer" switch-to-buffer)
      ("m" "files" (lambda () (interactive)
                     (let ((invalidate-cache (member "--invalidate-cache" (transient-args 'leader-main-menu))))
                       (projectile-find-file-dwim invalidate-cache))))
      ("M" "files (all)" (lambda () (interactive) (project-or-external-find-file t)))
      ("p" "project" hydra-submenu-project/body)]
     ["Other"
      ("H" "help" hydra-submenu-help/body)
      ("?" "help" hydra-submenu-help/body)
      ("f" "file" transient-file)
      ("c" "customize" hydra-submenu-customize-face/body)]]
    (interactive)
    (let ((transient-show-popup -0.2))
      (transient-setup 'leader-main-menu))))

;; `hydra-submenu-project' below `let'-binds `projectile-switch-project-action'
;; to rebind projectile's switch action per key. That variable is defined by
;; projectile (loaded lazily), so without this forward declaration it isn't
;; special when these forms load: under lexical binding the `let' binds it
;; lexically, the rebinding silently no-ops, and when projectile later loads its
;; `defcustom' errors "Defining as dynamic an already lexical var". Declaring it
;; special here (file-local, no value — projectile still sets the default) makes
;; the `let' dynamic.
(defvar projectile-switch-project-action)

(use-package hydra
  :config
  (defhydra hydra-submenu-leetcode (:exit t :hint nil)
    "
^LC^
^^^^^^^^----------------------------
  _l_: launch       _q_: quit
  _s_: submit     _e_/_t_: test
  _r_: reload "
    ("l" leetcode)
    ("q" leetcode-quit)
    ("s" leetcode-submit)
    ("e" leetcode-try)
    ("t" leetcode-try)
    ("r" leetcode-refresh))
  (defhydra hydra-submenu-eval (:exit t)
    ("e" duc/eval-dwim "dwim")
    ("b" duc/eval-buffer "buffer")
    ("p" duc/eval-print-dwim "print")
    ("P" duc/pretty-print-dwim "pretty print"))
  (defhydra hydra-submenu-claude (:exit t :hint nil)
    "
^Claude Code CLI^
^^^^^^^^----------------------------
  _w_: new worktree (mp-worktree-create)
  _d_: new session at directory
  _o_: open/create session
  _r_: resume session
  _a_: add session to bnote
  _l_: list all sessions "
    ("w" duc/mp-worktree-create)
    ("d" duc/claude-new-session-at-working-directory)
    ("o" duc/claude-open-or-create-terminal-session)
    ("r" duc/claude-resume-session)
    ("a" duc/claude-session-add-to-bnote)
    ("l" duc/claude-list-all-terminal-sessions))
  (defhydra hydra-submenu-agent-shell (:exit t :hint nil)
    "
^agent-shell^
^^^^^^^^----------------------------
  _c_: start Claude agent
  _x_: start Codex agent
  _s_: agent-shell (any agent)
  _b_: chat sidebar
  _r_: restart agent
  _h_: help menu "
    ("c" agent-shell-anthropic-start-claude-code)
    ("x" agent-shell-openai-start-codex)
    ("s" agent-shell)
    ("b" agent-sidebar-toggle-sidebar)
    ("r" agent-shell-restart)
    ("h" agent-shell-help-menu))
  (defhydra hydra-submenu-help (:exit t :hint nil)
    "
^Describe^           ^Info^
^^^^^^^^-------------------------------------
_m_: mode             _p_: list packages
_k_: key-to-func      _a_: apropos
_K_: func-to-key      _M_: search emacs manual
_s_: symbol
_f_: function
_v_: variable         _d_: toggle error debugging
_c_: face             _w_: watch function for step-debugging
_b_: bindings (list)  _W_: stop watching function for step-debugging
_B_: bindings
"
    ("m" describe-mode)
    ("s" describe-symbol)
    ("f" describe-function)
    ("v" describe-variable)
    ("k" describe-key)
    ("K" where-is)
    ("c" describe-face)
    ("b" describe-bindings)
    ("B" describe-bindings)
    ("p" package-list-packages)
    ("a" apropos-command)
    ("M" info-apropos)
    ("d" toggle-debug-on-error)
    ("w" debug-on-entry)
    ("W" cancel-debug-on-entry))
  (defhydra hydra-submenu-customize-face (:exit t :hint nil)
    "
^Font^                  ^Face^                 ^Buffer^
^^^^^^^^----------------------------------------------------------------
_f_: font            _c_: describe face     _h_: hex colors
_s_: font size       _t_: theme             _w_: whitespace
_+_: font scale +    ^ ^                    _l_: word-wrap
_-_: font scale -
_=_: font scale =
_W_: font weight cycle
_L_: font line spacing
"
    ("f" duc/set-font)
    ("s" duc/set-font-size)
    ("+" text-scale-increase :color red)
    ("-" text-scale-decrease :color red)
    ("=" (text-scale-mode -1) :color red)
    ("W" duc/font-weight-cycle :color red)
    ("L" duc/set-font-line-spacing)
    ("c" describe-face)
    ("t" duc/selectrum-load-theme)
    ("h" rainbow-mode)
    ("w" whitespace-mode)
    ("l" toggle-truncate-lines))
  (defhydra hydra-submenu-package (:exit t)
    ("l" package-list-packages-no-fetch "package-list"))
  (defhydra hydra-submenu-project (:exit t)
    ("n" (let ((projectile-switch-project-action #'projectile-switch-to-buffer))
           (projectile-switch-project)) "buffer")
    ("m" (let ((projectile-switch-project-action #'projectile-find-file))
           (projectile-switch-project)) "file")
    ("," (let ((projectile-switch-project-action #'consult-ripgrep))
           (projectile-switch-project)) "contents")
    ("p" (let ((projectile-switch-project-action #'projectile-vc))
           (projectile-switch-project)) "vc")
    ("v" (let ((projectile-switch-project-action #'projectile-vc))
           (projectile-switch-project)) "vc")
    ("g" (let ((projectile-switch-project-action #'projectile-vc))
           (projectile-switch-project)) "vc"))
  (defhydra hydra-submenu-git (:exit t :hint nil)
    "
              ^Git^
^^^^^^^^---------------------------------
_v_: status      _L_: log       _b_: blame
_c_: clone       _f_: file log
_j_: smerge next _u_: upper     _e_: smerge
_k_: smerge prev _l_: lower     _m_: smerge
_P_: 80-char sentences
"
    ("v" magit-status)
    ("c" duc/git-clone)
    ("L" magit-log)
    ("f" magit-log-buffer-file)
    ("b" magit-blame)
    ("j" smerge-next)
    ("k" smerge-prev)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("e" smerge-ediff)
    ("m" smerge-ediff)
    ("P" fill-paragraph))
  (defhydra hydra-submenu-anki (:exit t :hint nil)
    "
^anki-connect^            ^Media^
^^^^^^^^-------------------------------------------
_p_/_a_: push notes         _i_: screenshot
  _r_: retry push         _I_: url image
                        _c_: corpus (gutenberg)
  _n_: new note           _d_: dictionary
                        _s_: text-to-speech
"
    ("p" duc/anki-connect-push)
    ("a" duc/anki-connect-push)
    ("r" anki-editor-retry-failure-notes)
    ("n" (org-capture nil "1"))
    ("c" (consult-ripgrep "~/dev/notes/corpus"))
    ("d" osx-dictionary-search-word-at-point)
    ("i" org-download-screenshot)
    ("I" org-download-image)
    ("s" duc/forvo-text-to-sound-at-region-or-word)))

(use-package ace-window
  :config
  ; aw-keys are 0-9 by default, which is reasonable, but in the setup above,
  ; the keys are on the home row.
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package abbrev
  :ensure nil
  :config
  (setq save-abbrevs 'silent) ;; save abbrevs when files are saved
  :init
  (setq abbrev-file-name
        (expand-file-name "abbrev_defs.el" user-emacs-directory)))

;; Built-in tree-sitter (Emacs 31+), replacing the third-party `tree-sitter' /
;; `tree-sitter-langs' pair, which only did highlighting.
;; `treesit-enabled-modes' t copies every entry of
;; `treesit-major-mode-remap-alist' into `major-mode-remap-alist', so each
;; language opens in its FOO-ts-mode.  That copying is done by the option's
;; `:set' function, so the value has to be assigned after treesit.el is loaded
;; -- hence `:demand' (the require costs ~10ms) and `setopt' over `setq'.
;; `treesit-auto-install-grammar' defaults to `ask', which offers to build a
;; missing grammar into `user-emacs-directory'/tree-sitter on first visit.
;;
;; The ts modes derive from FOO-base-mode rather than FOO-mode, so the hooks
;; further down hang off the base mode where one exists (`sh-base-mode',
;; `js-base-mode', `python-base-mode') and name both modes where none does.
(use-package treesit
  :ensure nil
  :demand t
  :config
  (setopt treesit-enabled-modes t))

;; themes

(use-package flatland-black-theme
  :no-require t)

(use-package seoul256-theme
  :vc (:url "https://github.com/dangduc/seoul256-emacs")
  :no-require t
  :config
  (setq seoul256-background 256))

(use-package doom-themes
  :after ghostel
  :config
  (defun duc/theme-setup-doom-flatwhite-theme (&rest _)
    "Tweak ghostel display colors for doom-flatwhite"
    (let ((current-theme (car custom-enabled-themes)))
      (when (eq current-theme 'doom-flatwhite)
        (set-face-attribute 'ghostel-color-black nil
                            ;; "Normal" ansi color for foreground black (maybe).
                            :foreground "#7a7a7a"
                            ;; "Bright" ANSI color for foreground black (maybe).
                            :background "#a1a1a1"))))
  (advice-add 'load-theme :after #'duc/theme-setup-doom-flatwhite-theme))

(use-package solarized-theme)

(use-package catppuccin-theme)

(use-package modus-themes
  :init
  (setq modus-themes-deuteranopia t)

  (defun duc/theme-setup-modus-vivendi-theme (&rest _)
    "Tweak vterm display colors for doom-flatwhite"
    (let ((current-theme (car custom-enabled-themes)))
      ;; modus-themes v4 dropped the `modus-themes-hl-line' face; tweak the
      ;; standard `hl-line' face instead.
      (when (eq current-theme 'modus-vivendi)
        (set-face-attribute 'hl-line nil :background "#dedede"))))
  (advice-add 'load-theme :after #'duc/theme-setup-modus-vivendi-theme)

  (defun duc/theme-setup-modus-operandi-theme (&rest _)
    "Tweak vterm display colors for doom-flatwhite"
    (let ((current-theme (car custom-enabled-themes)))
      (when (eq current-theme 'modus-operandi)
        (set-face-attribute 'hl-line nil :background "#DEECF4"))))
  (advice-add 'load-theme :after #'duc/theme-setup-modus-operandi-theme))

(use-package mindre-theme
  :vc (:url "https://github.com/erikbackman/mindre-theme") ;; removed from MELPA
  :init
  (setq mindre-use-more-bold nil)
  (setq mindre-use-faded-lisp-parens t))

;(use-package fruity-theme
;  :straight (:host github
;                   :repo "jojojames/fruity-theme")
;  :init
;  (setq fruity-want-transparent-line-numbers t)
;  (setq fruity-want-dark-modeline t))

(use-package fruity-theme
  :ensure nil ;; local copy on load-path (vendor/fruity-theme)
  :load-path "vendor/fruity-theme"
  :init
  (setq fruity-want-transparent-line-numbers nil)
  (setq fruity-want-dark-modeline nil))

;; end themes

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package rainbow-delimiters) ;; GNU ELPA

(use-package habamax-theme
  :no-require t)

(use-package whitespace
  :ensure nil
  :diminish whitespace-mode
  :init
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs
                                trailing
                                empty
                                space-before-tab::tab
                                space-before-tab::space))
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package macrostep
  :commands (macrostep-expand macrostep-collapse)
  :init
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "C-;") 'macrostep-collapse)
    (define-key evil-normal-state-map (kbd "C-'") 'macrostep-expand)))

(use-package smartparens
  :no-require t
  :diminish smartparens-mode
  :init
  (dolist (hook '(lisp-mode-hook
                  scheme-mode-hook
                  clojure-mode-hook
                  emacs-lisp-mode-hook))
    (add-hook hook #'smartparens-strict-mode))

  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (setq sp-cancel-autoskip-on-backward-movement nil
        sp-autoskip-closing-pair 'always-end
        sp-autoskip-opening-pair t)

  :config
  ;; Disable highlights.
  ;(use-package smartparens-config)
  (smartparens-global-mode 1)
  (sp-pair "(" ")" :wrap "M-(")
  (sp-pair "(" ")" :wrap "M-)")
  (sp-pair "[" "]" :wrap "M-[")
  (sp-pair "[" "]" :wrap "M-]")
  (sp-pair "{" "}" :wrap "M-{")
  (sp-pair "{" "}" :wrap "M-}")
  (sp-pair "\"" "\"" :wrap "M-\"")
  ; Remove global policies.
  (sp-pair "`" nil :actions :rem)
  (sp-pair "'" nil :actions :rem)
  ; Add major-mode policies.
  (dolist (c '("'" "`"))
    (sp-local-pair '(typescript-mode
                     typescript-ts-mode
                     tsx-ts-mode
                     javascript-mode
                     js-ts-mode) c c)))

(use-package lispyville
  :diminish (lispyville-mode)
  :init
  (dolist (hook '(lisp-mode-hook
                  scheme-mode-hook
                  racket-mode-hook
                  clojure-mode-hook
                  emacs-lisp-mode-hook))
    (add-hook hook (lambda ()
                     (lispyville-mode))))
  :config
  (lispyville-set-key-theme
   '(operators
     s-operators
     slurp/barf-cp
     additional
     atom-movement
     prettify
     escape))
  (lispyville-mode))

(use-package ag
  :commands (ag ag-project ag-regexp ag-files))

(use-package deadgrep
  :commands (deadgrep))

(use-package projectile
  :commands (projectile-project-p
             projectile-project-root
             projectile-find-file
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-ag
             projectile-recentf)
  :diminish projectile-mode
  :init
  (when (eq system-type 'windows-nt)
    (setq projectile-indexing-method 'alien)
    (setq projectile-enable-caching t))
  (setq projectile-enable-caching t)
  :config
  (projectile-mode))

(use-package ibuffer-projectile
  :commands (ibuffer-projectile-set-filter-groups
             ibuffer-projectile-generate-filter-groups)
  :init
  (setq ibuffer-projectile-prefix "Project: ")
  (defun +ibuffer-projectile-run ()
    "Set up `ibuffer-projectile'."
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))

  (add-hook 'ibuffer-sidebar-mode-hook #'+ibuffer-projectile-run)
  (add-hook 'ibuffer-hook #'+ibuffer-projectile-run))

(use-package rainbow-delimiters
  :init
  (setq show-paren-delay 0)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :config
  (show-paren-mode 1))

;; consult provides the vertico-era replacements for the counsel/swiper
;; commands this config used (consult-line, consult-ripgrep, consult-find, ...).
(use-package consult
  :commands (consult-line
             consult-ripgrep
             consult-grep
             consult-find
             consult-buffer
             consult-imenu
             consult-project-buffer)
  :init
  (with-eval-after-load 'projectile
    (setq projectile-switch-project-action 'projectile-find-file)))

(use-package vertico
  ;; The vertico ELPA package bundles its extensions (vertico-directory etc.),
  ;; so no special recipe is needed under package.el.
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (vertico-mode))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package orderless
  :commands (orderless-filter))

;; `flx' is kept as a pure-elisp fallback scorer; the active scorer is the
;; native `fzf-native' batch path (see below).
(use-package flx)

;; Native fzf batch scorer, vendored fork in `vendor/fzf-native' (on `load-path'
;; via init.el). It ships prebuilt dynamic modules under `bin/' — on Apple
;; Silicon `fzf-native-load-dyn' picks `bin/Darwin/arm64/fzf-native-module.so'.
;; `fussy-setup-fzf' points fussy at the native `fussy-fzf-score'; the module is
;; loaded lazily on first completion via `fussy--ensure-fzf-loaded'.
(use-package fzf-native
  :ensure nil
  :defer t)

;; `fzfa' — async fuzzy pickers on top of `fzf-native'. Vendored fork in
;; `vendor/fzfa' (ahead of the archive build; see the load-path note in
;; init.el). A package.el install would hand us `fzfa-autoloads.el' at
;; activation, but a vendored tree gets no autoload file loaded for it, so the
;; stubs are declared here: one `use-package' per source file, mirroring the
;; `;;;###autoload' cookies in that tree. `make autoloads' in `vendor/fzfa'
;; regenerates the canonical list to re-mirror from when fzfa gains commands.
;; `:commands'/`:autoload' only define stubs, so every file stays off the
;; startup path; delete the forms for extensions you do not use.
(use-package fzfa
  :ensure nil
  :commands (fzfa-replay fzfa-find-any fzfa-find-some fzfa-passwords)
  :autoload (fzfa-completing-read fzfa-multi-read))

(use-package fzfa-ag
  :ensure nil
  :commands (fzfa-ag-files fzfa-ag))

(use-package fzfa-chrome
  :ensure nil
  :commands (fzfa-chrome-refresh fzfa-chrome-bookmarks fzfa-chrome-edit
             fzfa-chrome-bookmark-copy-url fzfa-chrome-history
             fzfa-chrome-history-copy-url fzfa-chrome-pass-refresh
             fzfa-chrome-pass-copy fzfa-chrome-pass-copy-username
             fzfa-chrome-pass-url)
  :autoload (fzfa-chrome-setup))

(use-package fzfa-company
  :ensure nil
  :commands (fzfa-company fzfa-company-show-doc fzfa-company-show-location)
  :autoload (fzfa-company-setup))

(use-package fzfa-eglot
  :ensure nil
  :commands (fzfa-eglot-symbols))

(use-package fzfa-emacs
  :ensure nil
  :commands (fzfa-recent-file fzfa-buffer fzfa-yank-pop fzfa-bookmark
             fzfa-theme fzfa-font fzfa-man fzfa-swiper fzfa-swiper-all
             fzfa-unicode-char fzfa-history fzfa-complex-command fzfa-M-x
             fzfa-M-x-for-buffer fzfa-apropos fzfa-descbinds
             fzfa-minor-mode-menu fzfa-mark fzfa-global-mark fzfa-register
             fzfa-outline fzfa-compile-error fzfa-ffap-menu fzfa-frames
             fzfa-tabs fzfa-browse-files))

(use-package fzfa-embark
  :ensure nil
  :autoload (fzfa-embark-setup))

(use-package fzfa-evil
  :ensure nil
  :commands (fzfa-evil-marks fzfa-evil-registers fzfa-evil-jumps
             fzfa-evil-ex-history fzfa-evil-search-history
             fzfa-evil-command-window fzfa-evil-any)
  :autoload (fzfa-evil-setup))

(use-package fzfa-fd
  :ensure nil
  :commands (fzfa-fd))

(use-package fzfa-find
  :ensure nil
  :commands (fzfa-find))

(use-package fzfa-firefox
  :ensure nil
  :commands (fzfa-firefox-refresh fzfa-firefox-bookmarks
             fzfa-firefox-bookmark-copy-url fzfa-firefox-history
             fzfa-firefox-history-copy-url)
  :autoload (fzfa-firefox-setup))

(use-package fzfa-flymake
  :ensure nil
  :commands (fzfa-flymake fzfa-flymake-project)
  :autoload (fzfa-flymake-setup))

(use-package fzfa-git
  :ensure nil
  :commands (fzfa-git-grep fzfa-git-ls-files fzfa-git-modified-locally
             fzfa-git-added-files fzfa-git-staged-for-commit
             fzfa-git-modified-in-head fzfa-git-log-grep))

(use-package fzfa-grep
  :ensure nil
  :commands (fzfa-grep fzfa-grep-current-file))

(use-package fzfa-helm
  :ensure nil
  :autoload (fzfa-helm-setup))

(use-package fzfa-hg
  :ensure nil
  :commands (fzfa-hg-files fzfa-hg-modified-locally fzfa-hg-added-files
             fzfa-hg-modified-in-head))

(use-package fzfa-hungry
  :ensure nil
  :commands (fzfa-hungry-swiper fzfa-hungry-find))

(use-package fzfa-imenu
  :ensure nil
  :commands (fzfa-imenu fzfa-imenu-all fzfa-imenu-all-but-current))

(use-package fzfa-info
  :ensure nil
  :commands (fzfa-info-emacs fzfa-info-elisp fzfa-info-org fzfa-info-cl
             fzfa-info-eieio fzfa-info-magit fzfa-info fzfa-info-at-point))

(use-package fzfa-ivy
  :ensure nil
  :autoload (fzfa-ivy-setup))

(use-package fzfa-loader
  :ensure nil
  :commands (fzfa-sync-autoloads))

(use-package fzfa-locate
  :ensure nil
  :commands (fzfa-locate))

(use-package fzfa-mail
  :ensure nil
  :commands (fzfa-mail-refresh fzfa-mail)
  :autoload (fzfa-mail-setup))

(use-package fzfa-make
  :ensure nil
  :commands (fzfa-make-reset-cache fzfa-make)
  :autoload (fzfa-make-setup))

(use-package fzfa-media-thumbnail
  :ensure nil
  :autoload (fzfa-media-thumbnail-setup))

(use-package fzfa-music
  :ensure nil
  :commands (fzfa-music-refresh fzfa-music-playlist
             fzfa-music-playlist-shuffle fzfa-music fzfa-music-by-artist
             fzfa-music-by-genre)
  :autoload (fzfa-music-setup))

(use-package fzfa-notmuch
  :ensure nil
  :commands (fzfa-notmuch fzfa-notmuch-tree fzfa-notmuch-show-thread
             fzfa-notmuch-tree-thread)
  :autoload (fzfa-notmuch-setup))

(use-package fzfa-org
  :ensure nil
  :commands (fzfa-org-heading fzfa-org-heading-all fzfa-org-agenda
             fzfa-org-todo fzfa-org-tags-view fzfa-org-insert-link
             fzfa-org-grep fzfa-org-files fzfa-org-mdfind-files
             fzfa-org-mdfind-grep fzfa-org-any))

(use-package fzfa-pass
  :ensure nil
  :commands (fzfa-pass-copy fzfa-pass-edit fzfa-pass-rename fzfa-pass-delete
             fzfa-pass-add fzfa-pass-generate fzfa-pass-url)
  :autoload (fzfa-pass-setup))

(use-package fzfa-posframe
  :ensure nil
  :commands (fzfa-posframe-mode))

(use-package fzfa-project
  :ensure nil
  :commands (fzfa-project-find-file fzfa-project-find-dir fzfa-project-buffer
             fzfa-project-recentf fzfa-project-switch-project))

(use-package fzfa-regexp
  :ensure nil
  :commands (fzfa-regexp))

(use-package fzfa-replay
  :ensure nil
  :commands (fzfa-replay-from-memory fzfa-replay-from-file fzfa-replay-any
             fzfa-replay-mode)
  :autoload (fzfa-replay-setup))

(use-package fzfa-rg
  :ensure nil
  :commands (fzfa-rg-files fzfa-rg))

(use-package fzfa-safari
  :ensure nil
  :commands (fzfa-safari-refresh fzfa-safari-bookmarks
             fzfa-safari-bookmark-copy-url fzfa-safari-history
             fzfa-safari-history-copy-url)
  :autoload (fzfa-safari-setup))

(use-package fzfa-shell
  :ensure nil
  :commands (fzfa-shell-command fzfa-shell-project-command fzfa-shell-history))

(use-package fzfa-spotlight
  :ensure nil
  :commands (fzfa-spotlight fzfa-spotlight-apps fzfa-spotlight-audio))

(use-package fzfa-tramp
  :ensure nil
  :commands (fzfa-ssh fzfa-tramp)
  :autoload (fzfa-tramp-setup))

(use-package fzfa-transient
  :ensure nil
  :commands (fzfa-transient))

(use-package fzfa-ugrep
  :ensure nil
  :commands (fzfa-ugrep))

(use-package fzfa-vc
  :ensure nil
  :commands (fzfa-vc-modified-files fzfa-vc-modified-locally
             fzfa-vc-added-files fzfa-vc-staged-for-commit
             fzfa-vc-modified-in-head fzfa-vc-any))

(use-package fzfa-vertico
  :ensure nil
  :commands (fzfa-vertico-columns-mode)
  :autoload (fzfa-vertico-setup))

(use-package fussy
  :after flx
  :config
  ;; Use fzf-native's multithreaded batch scorer (sets `fussy-score-ALL-fn' to
  ;; `fussy-fzf-score' and `fussy-filter-fn' to `fussy-filter-by-scoring').
  (fussy-setup-fzf)
  (setq
   ;; For example, project-find-file uses 'project-files which uses
   ;; substring completion by default. Set to nil to make sure it's using
   ;; fussy.
   completion-category-defaults nil
   completion-category-overrides nil)

  ;; Enable caching of results and filtering.
  (setq fussy-use-cache t)
  (advice-add 'company-auto-begin :before #'fussy-wipe-cache)

  ;; `eglot' defaults to flex, so set an override to point to fussy instead.
  (with-eval-after-load 'eglot
    (add-to-list 'completion-category-overrides
                 '(eglot (styles fussy basic)))))

(defun disable-company-mode-in-eshell-mode ()
  (company-mode -1))

;; inline / in-buffer completion
(use-package company
  :diminish company-mode
  :init
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  :config
  (defun d-company-capf-with-og-completion-styles (f &rest args)
    "Set `completion-styles' to be the default Emacs `completion-styles'
while `company-capf' runs."
    (let ((completion-styles '(basic substring flx)))
      (apply f args)))
  (advice-add 'company-capf :around 'd-company-capf-with-og-completion-styles)
  (defvar company-backends-original nil)
  (setq company-backends-original (or company-backends-original
                                      company-backends))
  (company-tng-mode)
  (global-company-mode))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package web-mode
  :mode
  ("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.blade\\.php\\'" . web-mode)
  ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.jsp\\'" . web-mode)
  ("\\.eex\\'" . web-mode)
  ("\\.tsx\\'" . web-mode)
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              ;; Set up indentation.
              (let ((n 2))
                (setq-local web-mode-markup-indent-offset n)
                (setq-local web-mode-css-indent-offset n)
                (setq-local web-mode-code-indent-offset n)
                (with-eval-after-load 'evil
                  (setq-local evil-shift-width n)))))
  ;; Use `company-dabbrev-code' with `web-mode'.
  (when (boundp 'company-dabbrev-code-modes)
    (push 'web-mode company-dabbrev-code-modes))

  (with-eval-after-load 'evil
    (evil-define-key 'normal web-mode-map
      (kbd "C-d") 'evil-scroll-down)))

(use-package jq-mode
  :mode
  ("\\.jq\\'" . jq-mode))

(use-package typescript-mode
  ;; npm install -g typescript
  :mode
  ("\\.ts\\'" . typescript-mode)
  ("\\.ts$\\'" . typescript-mode)
  :config
  (add-hook 'typescript-mode-hook
            (lambda ()
              (setq-local typescript-indent-level 2)
              (with-eval-after-load 'evil
                (setq-local evil-shift-width typescript-indent-level))))
  (setq typescript-enabled-frameworks '(typescript)))

;; `.ts' now lands in `typescript-ts-mode' via `major-mode-remap-alist', so the
;; `typescript-mode' hook above no longer runs.  `typescript-ts-indent-offset'
;; already defaults to 2; mirror it into `evil-shift-width' as that hook did.
(add-hook 'typescript-ts-base-mode-hook
          (lambda ()
            (with-eval-after-load 'evil
              (setq-local evil-shift-width typescript-ts-indent-offset))))

(use-package flycheck
  :init
  ; `sh-base-mode' covers both `sh-mode' and `bash-ts-mode'.
  (add-hook 'sh-base-mode-hook (lambda () (flycheck-mode 1)))
  (add-hook 'tide-mode-hook (lambda () (flycheck-mode 1))))

(use-package tide
  :config
  (setq tide-jump-to-definition-reuse-window nil)
  ;; Set up Typescript linting with `web-mode'.
  ;; https://github.com/ananthakumaran/tide/pull/161
  (with-eval-after-load 'flycheck
    ;; `typescript-tslint' was removed from flycheck (tslint is deprecated);
    ;; only register it if the checker is still defined.
    (when (flycheck-valid-checker-p 'typescript-tslint)
      (flycheck-add-mode 'typescript-tslint 'web-mode)))
  (defun +setup-tide-mode ()
    (interactive)
    (when (locate-dominating-file default-directory "tsfmt.json")
      (add-hook 'before-save-hook #'tide-format-before-save nil t))
    ;; Disable linting for Typescript Definition files.
    (when (and (buffer-file-name)
               (string-match-p ".d.ts$" (buffer-file-name)))
      (flycheck-mode -1))
    (tide-setup)
    (tide-hl-identifier-mode +1))
  (add-hook 'typescript-mode-hook #'+setup-tide-mode)
  (add-hook 'typescript-ts-base-mode-hook #'+setup-tide-mode)

  (with-eval-after-load 'evil
    (evil-define-key '(normal insert) tide-mode-map (kbd "M-?") 'tide-references))

  (add-hook 'web-mode-hook
            (lambda ()
              ;; Set up Tide mode if Typescript.
              (when (string-equal (file-name-extension buffer-file-name) "tsx")
                (setq-local web-mode-enable-auto-quoting nil)
                (when (fboundp 'yas-activate-extra-mode)
                  (yas-activate-extra-mode 'typescript-mode))
                (+setup-tide-mode)))))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :commands (restclient-mode))

(use-package magit
  :after transient
  :commands (magit-toplevel
             magit-status
             magit-blame
             magit-log
             magit-find-file
             magit-find-file-other-window)
  :init
  (setq magit-bury-buffer-function 'magit-mode-quit-window)

  (setq magit-diff-refine-hunk 'all)
  ;(setq magit-diff-arguments '("--no-ext-diff" "--stat" "-U5"))

  ;; Save buffers automatically instead of asking.
  (setq magit-save-repository-buffers 'dontask)

  ;; Popup the magit-process buffer if a command takes longer than n seconds.
  (setq magit-process-popup-time 5)

  (setq magit-repository-directories '("~/dev" "~/.emacs.d"))
  (setq magit-refresh-status-buffer nil)

  (setq magit-log-margin '(t "%b %d, %Y " magit-log-margin-width t 18))
  (setq magit-log-show-refname-after-summary t)

  :config
  ; Disable binding for blame when in a magit diff buffer.
  (define-key magit-blob-mode-map (kbd "b") nil)
  (define-key magit-diff-section-base-map (kbd "C-<return>") 'magit-diff-visit-worktree-file-other-window)

  (define-key magit-hunk-section-map (kbd "<return>") 'magit-diff-visit-file-other-window))

;; `evil-ediff' was merged into `evil-collection' (which supplies the ediff
;; keybindings now), so we only keep the plain ediff configuration here.
(use-package ediff
  :ensure nil
  :init
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options "-w")
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(use-package ibuffer-sidebar
  :init
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face '(:family duc/font-family :height 120)))

(use-package dired-subtree
  ; provided by dired-hacks.
  :commands (dired-subtree-toggle dired-subtree-cycle)
  :init
  (setq dired-subtree-use-backgrounds nil))

(use-package vscode-icon
  :vc (:url "https://github.com/jojojames/vscode-icon-emacs"))

(use-package dired-sidebar
  :after vscode-icon
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-width 30)
  (setq dired-sidebar-theme 'ascii)
  (setq dired-sidebar-face '(:family duc/font-family :height 120)))

(use-package all-the-icons
  :init
  (when (not local/all-the-icons-installed)
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :after dired-sidebar
  :commands (all-the-icons-dired-mode))

(use-package markdown-mode
  :commands
  (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package swift-mode
  :mode "\\.swift\\'")

(use-package kotlin-mode
  :mode ("\\.kt\\'" "\\.kts\\'"))

(use-package lsp-mode
  :init
  (setq lsp-log-io t)
  (setq lsp-lens-auto-enable nil)
  :hook ((c-mode . lsp)
         (c-ts-mode . lsp)
         (clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)))

(use-package ccls
  ;; Loaded lazily when a C-family buffer opens, so it registers its lsp client
  ;; without pulling lsp-mode in at startup.
  :defer t
  :hook ((c-mode c++-mode c-ts-mode c++-ts-mode objc-mode) . (lambda () (require 'ccls))))

; Python projects should initialize the python lsp themselves.
; using
;   (lsp) after require
(use-package lsp-python-ms
  ;; `:defer t' prevents use-package from inserting an eager load-time require
  ;; (it can't derive deferral from the lambda hook), which otherwise pulls in
  ;; lsp-mode at startup.
  :defer t
  :hook (python-base-mode . (lambda ()
                              (require 'lsp-python-ms))))

(setq python-indent-offset 2)

(use-package pyvenv
  :hook (python-base-mode . pyvenv-mode)
  :config
  (pyvenv-mode t)
  ;; Usage
  ;; 1. pyvenv-activate --> project-directory/venv
  ;; 2. C-c C-p to run inferior python.
  ;; 3. <leader> e e
  ;; 4. pyvenv-deactivate
  ;; https://stackoverflow.com/a/70371884

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package racket-mode
  :mode "\\.rkt\\'")

(use-package rainbow-mode)

(use-package vimrc-mode)

; Remove .json from using major mode
; Fixes issue where loading large json file freezes emacs.
(setq auto-mode-alist (rassq-delete-all 'javascript-mode auto-mode-alist))

(use-package ghostel
  :if (not (eq system-type 'windows-nt))
  :after general
  :commands (ghostel ghostel-project)
  :init
  ;; Unlike vterm, ghostel uses a prebuilt native module rather than a local
  ;; cmake build. The default `ask' pops an interactive prompt on first launch,
  ;; which blocks programmatic callers (see `duc/ivy-shell-send-string'); use
  ;; `download' to fetch the prebuilt binary from GitHub releases silently.
  (setq ghostel-module-auto-install 'download)
  (setq ghostel-kill-buffer-on-exit nil)
  ;; `ghostel-max-scrollback' is in BYTES (vterm's `vterm-max-scrollback' was
  ;; in lines). ~100MB is a very deep history, comparable to the old
  ;; 100000-line setting.
  (setq ghostel-max-scrollback (* 100 1024 1024))
  :config
  (general-define-key
   :keymaps 'ghostel-mode-map
   "M-k" 'ghostel-clear))

;; Evil integration for ghostel (analog of `evil-collection-vterm'). Ships in
;; the ghostel repo's `extensions/' dir and is published to MELPA separately.
(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode)
  :custom
  ;; Route insert-state ESC to evil (insert->normal) by default.  The package
  ;; default `auto' forwards ESC to the terminal only in alt-screen (DECSET
  ;; 1049), which is what made ESC feel eaten at a plain prompt.  `evil' is
  ;; unconditional and predictable; cycle to `terminal' per-buffer with
  ;; `M-<escape>' when a program (zsh vi-mode, a TUI) needs the raw ESC.
  (evil-ghostel-escape 'evil)
  :config
  ;; Bind the toggle in evil's insert/normal state maps, not `ghostel-mode-map'.
  ;; ghostel's semi-char *local* map forwards every `M-<key>' to the terminal
  ;; (via `ghostel--send-event'), shadowing `ghostel-mode-map'.  Evil's state
  ;; keymaps live in `emulation-mode-map-alists', which outrank the local map,
  ;; so binding here lets the evil-mode binding win as desired.
  (general-define-key
   :keymaps 'evil-ghostel-mode-map
   :states '(insert normal)
   "M-<escape>" 'evil-ghostel-toggle-send-escape)
  ;; Make `p' / `P' paste into an alt-screen program (tmux, vim, a pager).
  ;; evil-ghostel routes paste to the PTY only at a shell prompt; in alt-screen
  ;; it falls back to `evil-paste-after', which errors "Buffer is read-only" on
  ;; the renderer-owned buffer.  Redirect that fallback to a bracketed paste.
  (advice-add 'evil-ghostel-paste-after :around
              #'duc/ghostel--evil-paste-to-terminal)
  (advice-add 'evil-ghostel-paste-before :around
              #'duc/ghostel--evil-paste-to-terminal))

(use-package agent-shell-sidebar
  :ensure nil
  :commands (agent-shell-sidebar-toggle-sidebar
             agent-shell-sidebar-show-sidebar
             agent-shell-sidebar-hide-sidebar
             agent-shell-sidebar-jump-to-sidebar))

(use-package agent-sidebar
  :ensure nil
  :commands (agent-sidebar-toggle-sidebar
             agent-sidebar-show-sidebar
             agent-sidebar-hide-sidebar
             agent-sidebar-jump-to-sidebar))

;; `agent-shell' — an in-Emacs shell for coding agents over the Agent Client
;; Protocol (ACP), configured for the Claude agent.  It drives Claude through
;; the external `claude-agent-acp' bridge (install once with
;;   npm install -g @agentclientprotocol/claude-agent-acp
;; ), authenticating with the Claude subscription login rather than an API key.
;; Deps `acp' and `shell-maker' install from MELPA automatically.  Adapted from
;; xenodium's README example and a shared community config.
(use-package agent-shell
  :commands (agent-shell
             agent-shell-help-menu
             agent-shell-anthropic-start-claude-code
             agent-shell-openai-start-codex)
  :config
  (require 'cl-lib)
  (require 'map)
  (setq agent-shell-session-restore-verbosity 'full)
  (setq agent-shell-confirm-interrupt nil)
  ;; Reuse the Claude subscription login; no API key stored in the config.
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))

  (defun duc/agent-shell--split-shell-toplevel (command)
    "Split COMMAND on shell operators &&, ||, ;, | at top level only.
Operators inside single or double quotes are left intact."
    (let ((segments nil)
          (start 0)
          (i 0)
          (len (length command))
          (in-single nil)
          (in-double nil))
      (cl-flet ((emit (end)
                  (let ((seg (string-trim (substring command start end))))
                    (unless (string-empty-p seg)
                      (push seg segments)))))
        (while (< i len)
          (let ((c (aref command i)))
            (cond
             ((and (eq c ?\\) (< (1+ i) len))
              (cl-incf i 2))
             ((and (not in-double) (eq c ?\'))
              (setq in-single (not in-single)) (cl-incf i))
             ((and (not in-single) (eq c ?\"))
              (setq in-double (not in-double)) (cl-incf i))
             ((or in-single in-double)
              (cl-incf i))
             ((and (or (eq c ?&) (eq c ?|))
                   (< (1+ i) len)
                   (eq (aref command (1+ i)) c))
              (emit i) (cl-incf i 2) (setq start i))
             ((or (eq c ?\;) (eq c ?|))
              (emit i) (cl-incf i) (setq start i))
             (t (cl-incf i)))))
        (emit i))
      (nreverse segments)))

  (defun duc/agent-shell-can-auto (permission)
    "Auto-approve safe PERMISSION requests; return nil to fall back to prompt.
Finds, reads, searches, and fetches are always allowed.  Shell commands are
allowed only when every top-level segment matches a read-only allowlist.
Edits, writes, and everything else return nil so `agent-shell' shows its
interactive permission dialog.  See `agent-shell-permission-responder-function'."
    (cl-labels
        ((allow-once ()
           (when-let* ((choice
                        (seq-find
                         (lambda (option)
                           (equal (map-elt option :kind) "allow_once"))
                         (map-elt permission :options))))
             (funcall (map-elt permission :respond)
                      (map-elt choice :option-id))
             t)))
      (let* ((tool-call (map-elt permission :tool-call))
             (kind (map-elt tool-call :kind)))
        (pcase kind
          ("find"
           (prog1 (allow-once)
             (message "auto-reading: %s" (map-elt tool-call :path))))
          ("read"
           (prog1 (allow-once)
             (message "auto-reading: %s" (map-elt tool-call :path))))
          ("search"
           (prog1 (allow-once)
             (message "auto-searching: %s" (map-elt tool-call :title))))
          ("fetch"
           (prog1 (allow-once)
             (message "auto-fetching: %s" (map-elt tool-call :title))))
          ("execute"
           (let* ((command (map-elt tool-call :command))
                  (safe-segment-rx
                   (rx bos
                       (or
                        (seq (or "cmake" "make" "grep" "rg" "wc"
                                 "head" "tail" "cat" "ls" "find"
                                 "echo" "pwd" "file" "which" "type"
                                 "curl")
                             (or eos (any " \t")))
                        (seq "git "
                             (or "show" "log" "diff" "status" "blame"
                                 "ls-files" "rev-parse" "branch"
                                 "describe" "config --get"))
                        (seq "cd /Users/ducnguyen/.emacs.d")
                        "sed -n")))
                  (segments (duc/agent-shell--split-shell-toplevel command))
                  (all-safe (and segments
                                 (cl-every
                                  (lambda (seg)
                                    (string-match-p safe-segment-rx seg))
                                  segments))))
             (if all-safe
                 (progn
                   (message "auto-allowing: %s" command)
                   (allow-once))
               (message "agent-shell: permission UI for: %s" command)
               nil)))
          (_
           (message "agent-shell: permission UI for %s" kind)
           nil)))))

  (setq agent-shell-permission-responder-function #'duc/agent-shell-can-auto)

  ;; `global-company-mode' is on, so drive `agent-shell''s @/ completion through
  ;; company instead of its built-in `post-self-insert-hook' trigger.
  (defun duc/agent-shell-maybe-company-complete ()
    "Begin company completion when @ or / is typed at a word boundary.
Only fires at line start or after whitespace, avoiding spurious completions
mid-word or in paths.  A company-based replacement for
`agent-shell--trigger-completion-at-point'."
    (when (and (memq (char-before) '(?@ ?/))
               (or (= (point) (1+ (line-beginning-position)))
                   (memq (char-before (1- (point))) '(?\s ?\t ?\n))))
      (cond
       ((eq (char-before) ?@)
        (company-manual-begin))
       ((and (eq (char-before) ?/)
             (agent-shell--command-completion-at-point))
        (company-manual-begin)))))

  (defun duc/agent-shell-setup-completion ()
    "Swap `agent-shell''s @/ completion trigger for a company-based one."
    (remove-hook 'post-self-insert-hook
                 #'agent-shell--trigger-completion-at-point t)
    (add-hook 'post-self-insert-hook
              #'duc/agent-shell-maybe-company-complete nil t))

  (add-hook 'agent-shell-mode-hook #'duc/agent-shell-setup-completion))

(use-package tex
  :ensure auctex
  :defer t
  :init
  (setq org-format-latex-options
        '(:foreground default :background default :scale 1.7
                      :html-foreground "Black" :html-background "Transparent"
                      :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (setq TeX-engine "xelatex")
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

(use-package leetcode
  :commands (leetcode)
  :init
  (setq leetcode-prefer-language "python3")
  :config
  (evil-define-key 'normal tabulated-list-mode-map (kbd "RET") 'leetcode-show-current-problem))

(use-package ereader
  :mode
  ("\\.epub\\'" . ereader-mode)
  :config
  (evil-define-key 'normal ereader-mode-map (kbd "0") 'evil-digit-argument-or-evil-beginning-of-line))

(use-package asy-mode
  :after org-contrib
  ;; Vendored single file from the asymptote repo (vendor/asy-mode/), on load-path.
  :ensure nil)

(use-package org
  :ensure nil
  :config
  ;; org-mode

  (setq org-image-actual-width nil) ; Set inline display width of images.

  ;; [[https://beorgapp.com/learning/emacs-encryption/][Getting started with encryption in Org mode on macOS]].
  (require 'org-crypt)
  (require 'epa-file)
  (epa-file-enable)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key "duc")
  (setq auto-save-default nil)

  ;; Point to emacs source directory. This is typically the `src` directory of
  ;; the emacs repository [[https://github.com/emacs-mirror/emacs]].
  ;; [[Re: on specifying the C source code directory][https://lists.gnu.org/archive/html/help-gnu-emacs/2016-02/msg00007.html]].
  (setq find-function-C-source-directory (concat "~/dev/emacs-" emacs-version "/src"))

  ;; Display full link syntax (e.g. [[https://orgmode.org][Org website]]).
  (setq org-link-descriptive nil)

  ;; org-babel
  (setq org-ditaa-jar-path "~/.emacs.d/vendor/not-elisp/ditaa0_9.jar")
  (setq org-plantuml-jar-path "~/.emacs.d/vendor/not-elisp/plantuml.jar")

  (setq org-default-notes-file "~/dev/notes/notes.org")
  (setq org-capture-templates
        '(("D" "drill" entry (file+datetree "") "* %<%H%M:%S> %^{question} :drill:\n** Answer\n%^{answer}"
           :immediate-finish t)
          ("c" "(Quick) note" entry (file+datetree "") "* %<%H%M:%S> %^{note}\n  %l"
           :immediate-finish t)
          ("C" "Multi-line note" entry (file+datetree "") "* %<%H%M:%S> %?\n  %l")
          ("t" "TODO" entry (file+datetree "") "* TODO %<%H%M:%S> %^{todo}"
           :immediate-finish t)
          ("r" "Region" entry (file+datetree "") "* %<%H%M:%S> %(concat duc/org-code-block-filename)\n#+begin_src %(concat duc/org-code-block-language)\n%i\n#+end_src\n%(concat duc/org-code-block-link)"
           :immediate-finish t)
          ("n" "org-fc Normal" entry (file "~/dev/org-fc/unsorted.org") "* Normal Card\n%^{Front}\n** Back\n%^{Back}\n** Extra"
           :immediate-finish t)
          ("2" "Anki - Basic" entry (file+datetree "") "* %<%H%M:%S> English Definition :anki:\n:PROPERTIES:\n:ANKI_DECK: Default\n:ANKI_NOTE_TYPE: Basic\n:ANKI_TAGS: english definition\n:END:\n** Front\n** Back")
          ("1" "Anki - Word Pronunciation" entry (file+datetree "") "* %<%H%M:%S> English Pronunciation - %^{word} :anki:\n:PROPERTIES:\n:ANKI_DECK: Default\n:ANKI_NOTE_TYPE: Word-Pronunciation\n:ANKI_TAGS: english pronunciation\n:END:\n** Word\n%\\1\n** Picture\n** Sound\n** Pronunciation")))

                                        ; Don't indent by level. (Region-= will remove indents.)
  (setq org-adapt-indentation nil)
                                        ; Also don't intent src blocks.
                                        ; Org 9.8 (Emacs 31) renamed this to
                                        ; `org-src-content-indentation'; the old
                                        ; name is an obsolete alias there and the
                                        ; only name in Org 9.7 (Emacs 30), so pick
                                        ; whichever this Org defines.
  (set (if (boundp 'org-src-content-indentation)
           'org-src-content-indentation
         'org-edit-src-content-indentation)
       0))

;; Modern emacsql (GNU ELPA) has built-in SQLite support; the separate
;; `emacsql-sqlite-builtin' package is folded in and no longer needed.
;; Deferred: org-roam requires it lazily, so it stays off the startup path.
(use-package emacsql
  :defer t)

(use-package org-roam
  ;; Load on first roam command instead of at startup; DB autosync (the ~2s
  ;; startup cost) is moved to an idle timer so it runs shortly after init
  ;; without blocking it.
  :commands (org-roam-node-find org-roam-node-insert org-roam-capture
             org-roam-buffer-toggle org-roam-db-sync org-roam-dailies-capture-today)
  :init
  (setq org-roam-database-connector 'sqlite-builtin)
  (let ((d "~/dev/rotes"))
    (unless (file-exists-p d)
      (make-directory d))
    (setq org-roam-directory (file-truename d)))
  (run-with-idle-timer
   1 nil (lambda () (require 'org-roam) (org-roam-db-autosync-mode)))
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :vc (:url "https://github.com/org-roam/org-roam-ui" :branch "main")
  :after org-roam
  ;; normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;; a hookable mode anymore, you're advised to pick something yourself
  ;; if you don't care about startup time, use
  ;;    :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-download
  :init
  (setq org-download-annotate-function (lambda (link) ""))
  (setq org-download-image-dir "~/dev/notes/img")
  (setq org-download-screenshot-method
        (pcase system-type
          ('gnu/linux "scrot -s %s")
          (_ "screencapture -i %s")))
  (setq org-download-image-org-width 400))

(use-package org-ql
  :commands (org-ql-search org-ql-select org-ql-query org-ql-view))

(use-package org-fc
  :vc (:url "https://git.sr.ht/~l3kn/org-fc")
  ;; org-fc's algo classes inherit `eieio-singleton', which lives in eieio-base.
  :init
  (require 'eieio-base)
  ;; Work around an org-fc bug: `org-fc-algo-noop.el' is missing its
  ;; `org-fc-core'/`eieio-base' requires (unlike `org-fc-algo-sm2.el'), so
  ;; package.el compiles it standalone into a broken .elc/.eln that fails to
  ;; bind the class as a variable -> "void-variable org-fc-algo-noop" at load.
  ;; Pre-load the algo from source (which works) and `provide' it so org-fc's
  ;; own `(require 'org-fc-algo-noop)' skips the broken compiled file.
  (require 'org-fc-core)
  (let ((load-suffixes '(".el")))
    (load "org-fc-algo-noop" nil t))
  (let ((dir "~/dev/org-fc"))
    (unless (file-exists-p dir)
      (make-directory dir))
    (setq org-fc-directories `(,dir))
    (setq org-fc-review-history-file (expand-file-name "org-fc-reviews.tsv" dir)))
  :config
  (set-face-attribute 'org-fc-type-cloze-hole-face nil :foreground "blue"
                      :weight 'unspecified)
  (require 'org-fc-hydra)

  (define-key org-fc-review-flip-mode-map (kbd "RET") nil)
  (define-key org-fc-review-flip-mode-map (kbd "q") nil)
  (define-key org-fc-review-flip-mode-map (kbd "p") nil)
  (define-key org-fc-review-flip-mode-map (kbd "s") nil)

  (define-key org-fc-review-rate-mode-map (kbd "a") nil)
  (define-key org-fc-review-rate-mode-map (kbd "h") nil)
  (define-key org-fc-review-rate-mode-map (kbd "g") nil)
  (define-key org-fc-review-rate-mode-map (kbd "e") nil)
  (define-key org-fc-review-rate-mode-map (kbd "s") nil)
  (define-key org-fc-review-rate-mode-map (kbd "p") nil)
  (define-key org-fc-review-rate-mode-map (kbd "q") nil)

  (define-key org-fc-review-edit-mode-map (kbd "C-c C-c") nil)
  (define-key org-fc-review-edit-mode-map (kbd "C-c C-k") nil)

  (transient-define-prefix transient-org-fc-review-flip ()
    "org-fc"
    [["Review"
      ("RET" "flip" org-fc-review-flip)
      ("i" "edit" org-fc-review-edit)
      ("q" "quit" org-fc-review-quit)
      ("s" "suspend" org-fc-review-suspend-card)]
     ["Other"
      ("m" "dashboard" org-fc-dashboard)
      ("S" "screenshot" (lambda () (interactive)
                          (let ((org-download-image-dir "~/dev/org-fc/img"))
                            (org-download-screenshot))))
      ("h" "hydra" org-fc-hydra/body)]])

  (transient-define-prefix transient-org-fc-review-rate ()
    "org-fc"
    [["Rate"
      ("a" "again" org-fc-review-rate-again)
      ("1" "easy" org-fc-review-rate-easy)
      ("2" "good" org-fc-review-rate-good)
      ("3" "hard" org-fc-review-rate-hard)]
     ["Other"
      ("m" "dashboard" org-fc-dashboard)
      ("S" "screenshot" (lambda () (interactive)
                          (let ((org-download-image-dir "~/dev/org-fc/img"))
                            (org-download-screenshot))))
      ("h" "hydra" org-fc-hydra/body)]])

  (transient-define-prefix transient-org-fc-review-edit ()
    "org-fc"
    [["Edit"
      ("r" "resume review of card" org-fc-review-resume)
      ("q" "quit review entirely" org-fc-review-quit)]
     ["Other"
      ("m" "dashboard" org-fc-dashboard)
      ("S" "screenshot" (lambda () (interactive)
                          (let ((org-download-image-dir "~/dev/org-fc/img"))
                            (org-download-screenshot))))
      ("h" "hydra" org-fc-hydra/body)]])

  (push '(org-fc-review-flip-mode . transient-org-fc-review-flip) g-mode-alist)
  (push '(org-fc-review-rate-mode . transient-org-fc-review-rate) g-mode-alist)
  (push '(org-fc-review-edit-mode . transient-org-fc-review-edit) g-mode-alist))

(use-package anki-editor
  :commands (anki-editor-mode anki-editor-push-notes)
  :init
  (setq anki-editor-org-tags-as-anki-tags nil)
  (setq request-log-level 'debug))

(use-package osx-dictionary
  :commands (osx-dictionary-search-word-at-point osx-dictionary-search-input))

(use-package elfeed
  :commands (elfeed)
  :config
  (setq elfeed-feeds local/elfeed-feeds))

(use-package geiser
  :commands (geiser run-geiser geiser-mode)
  :init
  (setq geiser-mode-company-p nil)
  :config
  ; Have evil's normal mode handle last-sexp ending at point, as
  ; you would expect it to.
  ; e.g.   (list 'one 'two 'three)
  ;                              ^ point is here.
  ; With advice, will evaluate: (one two three)
  ;                 instead of: three
  ;
  ; See evil-collection-geiser-setup.
  (with-eval-after-load 'evil
    (with-eval-after-load 'duc
      (unless evil-move-beyond-eol
        (advice-add 'duc/geiser-eval-last-sexp :around 'evil-collection-geiser-last-sexp)))))

(use-package clojure-mode
  :mode ("\\.clj\\'" "\\.cljs\\'" "\\.cljc\\'" "\\.edn\\'"))

(use-package geiser-mit
  :after geiser)

(use-package lua-mode
  :mode "\\.lua\\'")
(use-package outline-indent)

;; Local working copies under ~/dev; loaded only when present.
(use-package rpgdm
  :ensure nil
  :if (file-directory-p "~/dev/emacs-rpgdm")
  :load-path "~/dev/emacs-rpgdm")

(use-package rpgdm-ironsworn
  :ensure nil
  :if (file-directory-p "~/dev/emacs-ironsworn")
  :load-path "~/dev/emacs-ironsworn"
  ;; rpgdm-ironsworn.el calls `f-join' at load time but doesn't require `f';
  ;; it used to work only because a now-deferred package pulled `f' in early.
  :init
  (require 'f)
  (setq rpgdm-ironsworn-project (expand-file-name "~/dev/emacs-ironsworn")))

(provide 'package-declarations)
