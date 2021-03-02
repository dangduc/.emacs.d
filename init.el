(defvar before-user-init-time (current-time)
  "Value of `current-time' when Emacs begins loading `user-init-file'.")
(message "Loading Emacs...done (%.3fs)"
         (float-time (time-subtract before-user-init-time
                                    before-init-time)))

(setq ring-bell-function #'ignore)

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq message-log-max 16384
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 402653184
                   gc-cons-percentage .3)
             (add-hook
              'focus-out-hook
              (lambda ()
                "Lower `gc-cons-threshold' and then run `garbage-collect'."
                (let ((gc-cons-threshold 800000))
                  (garbage-collect))))) t)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Get rid of extraneous UI
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Set title of window to current file or buffer name if not a file.
(setq frame-title-format
      '(""(:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

;; No need to type out full 'yes' / 'no' on confirmation prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; don't recenter when scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; display columns position in mode-line
(column-number-mode t)

; line numbers (emacs 26 and above)
(when (fboundp 'display-line-numbers-mode)
  (dolist (hook '(prog-mode-hook
                  nroff-mode-hook
                  nxml-mode-hook
                  conf-space-mode-hook))
    (add-hook hook
              (lambda ()
                (when (boundp 'display-line-numbers-widen)
                  (setq-default display-line-numbers-widen t))
                (set-face-attribute 'line-number-current-line nil :weight 'bold)
                (when (boundp 'display-line-numbers-type)
                  (setq display-line-numbers-type 't))
                (display-line-numbers-mode)))))

;; nowrap
(set-default 'truncate-lines t)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid))
                                          temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; Don't let osx swallow Meta key.
(setq mac-pass-command-to-system nil)

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; Start emacs server
;; e.g. ~/.zshrc
;;   # (find-file) send to emacs server.
;;   # ie, emacsclient -n file1 file2 ...
;;   alias emacsff="emacsclient -n"
(if (not (eq system-type 'windows-nt))
    (and window-system (server-start)))

;; org-mode

(setq org-image-actual-width 400) ; Set inline display width of images.

;; [[https://beorgapp.com/learning/emacs-encryption/][Getting started with encryption in Org mode on macOS]].
(require 'org-crypt)
(require 'epa-file)
(epa-file-enable)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
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

(defun decision-note-template ()
  "From Decision Checklist, Sam Kyle"
  (replace-regexp-in-string "#" (org-id-new)
                            "* %<%H%M:%S> Decision NO. #%?
(C-c i  to clock-in, C-c o  to clock-out.)
Decision:

Mental/Physical State (C-c C-x C-b  to toggle checkboxes):
- [ ] Energized
- [ ] Focused
- [ ] Relaxed
- [ ] Confident
- [ ] Tired
- [ ] Accepting
- [ ] Accommodating
- [ ] Anxious
- [ ] Resigned
- [ ] Frustrated
- [ ] Angry
The situation/context:

The problem statement or frame:

The variables that govern the situation include:

** TODO Review decision
(C-c C-d to insert deadline)
"))
(setq org-default-notes-file "~/dev/notes/log.org")
(setq org-capture-templates
      (quote (("D" "drill" entry (file+datetree "") "* %<%H%M:%S> %^{question} :drill:\n** Answer\n%^{answer}"
               :immediate-finish t)
              ("c" "(Quick) note" entry (file+datetree "") "* %<%H%M:%S> %^{note}\n  %l"
               :immediate-finish t)
              ("C" "Multi-line note" entry (file+datetree "") "* %<%H%M:%S> %?\n  %l")
              ("d" "Decision Template" entry (file+datetree "")
               (function decision-note-template))
              ("t" "TODO" entry (file+datetree "") "* TODO %<%H%M:%S> %^{todo}"
               :immediate-finish t)
              ("r" "Region" entry (file+datetree "") "* %<%H%M:%S> %(concat filename)\n#+begin_src %(concat language)\n%i\n#+end_src\n%(concat link)"
               :immediate-finish t)
              ("2" "Anki - Basic" entry (file+datetree "") "* %<%H%M:%S> English Definition :anki:\n:PROPERTIES:\n:ANKI_DECK: Default\n:ANKI_NOTE_TYPE: Basic\n:ANKI_TAGS: english definition\n:END:\n** Front\n** Back")
              ("1" "Anki - Word Pronunciation" entry (file+datetree "") "* %<%H%M:%S> English Pronunciation - %^{word} :anki:\n:PROPERTIES:\n:ANKI_DECK: Default\n:ANKI_NOTE_TYPE: Word-Pronunciation\n:ANKI_TAGS: english pronunciation\n:END:\n** Word\n%\\1\n** Picture\n** Sound\n** Pronunciation"))))

; Don't indent by level. (Region-= will remove indents.)
(setq org-adapt-indentation nil)

;; Misc
;;

;; Create buffer per occur.
(add-hook 'occur-hook
          (lambda ()
            (occur-rename-buffer)))
;; Don't confirm to follow links.
;; e.g. "Symbolic link to Git-controlled source file; follow link? (y or n)"
(setq vc-follow-symlinks t)

;; load path
;;

(load "~/.emacs.d/local-declarations.el")
(load "~/.emacs.d/local.el")

;; Stolen from [Aaron Bedra's Emacs 26 Configuration](http://aaronbedra.com/emacs.d/#vendor-directory)
;; Setup up vendor directory.
(defvar duc/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path duc/vendor-dir)

(dolist (project (directory-files duc/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Vendor management
;;

; n/a

;; package management
;;

(when (< emacs-major-version 27)
  (setq package-enable-at-startup nil)
  ;; (package-initialize)
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

(require  'use-package)
(setq use-package-verbose nil)

;; Package declarations
;;

(use-package duc
  :init
  (defvar duc/duc-dir (expand-file-name "duc" user-emacs-directory))
  (add-to-list 'load-path duc/duc-dir)
  :config
  ;; enable transparent osx titlebar (a la Chrome)
  (duc/alist-replace-set default-frame-alist (ns-transparent-titlebar . t))

  ;; nil or dark, to switch to between black or white title text
  ;; e.g. (duc/alist-replace-set default-frame-alist (ns-appearance . dark|nil))
  (duc/alist-replace-set default-frame-alist (ns-appearance . nil))

  (duc/alist-replace-set default-frame-alist (ns-use-thin-smoothing . t))
  (duc/alist-replace-set default-frame-alist (ns-antialias-text . nil))

  ;; Set font
  (set-face-attribute 'default nil
                      :family duc/font-family
                      :height duc/font-height
                      :weight duc/font-weight
                      :width 'normal)

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

  (defun duc/org-capture-region-with-code-block ()
    (interactive)
    (let ((filename (format "%s::%d" (buffer-name) (line-number-at-pos)))
          (link (duc/org-link-create-filename-line-number))
          (language (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))
      (org-capture nil "r")))

  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local normal-auto-fill-function 'bnote-auto-fill-function)
              (auto-fill-mode t)
              (setq-local truncate-lines t))))

(with-eval-after-load 'evil
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-inactive-mode-map [escape] 'minibuffer-keyboard-quit))

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
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'tide-mode-hook 'hs-minor-mode)
  (add-hook 'typescript-mode-hook 'hs-minor-mode)
  (add-hook 'kotlin-mode-hook 'hs-minor-mode)
  (add-hook 'swift-mode-hook 'hs-minor-mode)
  (add-hook 'js-mode-hook 'hs-minor-mode)
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
    (evil-collection-init))

(use-package general
  :after evil
  :config
  (general-override-mode)
  (general-define-key
   :states '(normal motion visual emacs)
   :keymaps 'override
   "<SPC>" 'hydra-main-menu/body)
  (with-eval-after-load 'duc
    ; https://en.wikipedia.org/wiki/Interpunct#In_mathematics_and_science
    (general-define-key
     :states '(insert emacs)
     :keymaps 'override
     "s-(" #'(lambda () (interactive) (insert "◼")))
    (general-define-key
     :states '(insert emacs)
     :keymaps 'override
     "s-)" #'(lambda () (interactive) (insert "◻")))
    (general-define-key
     :states '(normal insert motion visual emacs)
     :keymaps 'override
     "s-T" #'(lambda () (interactive) (duc/add-bnote-with-char "◼")))
    (general-define-key
     :states '(normal insert motion visual emacs)
     :keymaps 'override
     "s-E" #'(lambda () (interactive) (duc/add-bnote-with-char "◻")))
    (general-define-key
     :states '(normal insert motion visual emacs)
     :keymaps 'override
     "s-N" #'(lambda () (interactive) (duc/add-bnote-with-char "-")))
    (general-define-key
     :states '(normal insert motion visual emacs)
     :keymaps 'override
     "s-L" #'(lambda () (interactive) (duc/insert-bnote-lozenge-empty-link)))))

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
  (defhydra hydra-submenu-buffer (:exit t)
    ("p" previous-buffer "prev buffer")
    ("n" next-buffer "next buffer")
    ("l" list-buffers "list buffers")
    ("N" duc/new-buffer "new buffer")
    ("c" duc/new-buffer "new buffer")
    ("o" switch-to-buffer "switch buffer")
    ("r" revert-buffer "reload buffer")
    ("w" save-buffer "save buffer")
    ("t" auto-revert-tail-mode "tail -f")
    ("k" kill-buffer "kill buffer"))
  (defhydra hydra-submenu-eval (:exit t)
    ("e" duc/eval-dwim "dwim")
    ("b" duc/eval-buffer "buffer")
    ("p" duc/eval-last "at point")
    ("P" eval-print-last-sexp "print"))
  (defhydra hydra-submenu-window (:exit t :hint nil)
    "
^Frame^             ^Window^
^^^^^^^^----------------------------
_f_/_w_: maximize
  _n_: next         _b_: balance
  _N_: new          _k_: delete
                    _p_: pin/unpin buffer
  _
"
    ("f" toggle-frame-maximized)
    ("w" toggle-frame-maximized)
    ("n" other-frame)
    ("N" make-frame-command)
    ("b" balance-windows)
    ("k" delete-window)
    ("p" duc/toggle-pin-buffer))
  (defhydra hydra-submenu-file (:exit t)
    ("f" find-file "find file")
    ("w" save-buffer "write file")
    ("i" (find-file "~/.emacs.d/init.el" ) "init.el")
    ("I" (find-file "~/.emacs.d/duc/duc.el" ) "duc.el")
    ("b" duc/create-or-open-bnote "bnote")
    ("1" (find-file "~/dev/notes/index.org" ) "index.org")
    ("2" (find-file "~/dev/notes/how-to.org" ) "how-to.org")
    ("3" shell-command-on-region "M-|") ;; e.g. "nc termbin.com 9999"
    ("d" (org-capture nil "d") "capture decision")
    ("D" (org-capture nil "D") "capture drill")
    ("c" (org-capture nil "c") "capture note")
    ("C" (org-capture nil "C") "capture longer note")
    ("t" (org-capture nil "t") "capture todo")
    ("r" (duc/org-capture-region-with-code-block) "capture region")
    ("z" duc/create-linked-note "create linked note")
    ("Z" (duc/create-linked-note "~/dev/chrestoturing/") "create chrestoturing note")
    ("T" (multi-occur-in-matching-buffers "log.org" "\\*\\*\\*\\* TODO") "View TODOs")
    ("s" duc/sidebar-toggle "sidebar"))
  (defhydra hydra-submenu-help (:exit t :hint nil)
    "
^Describe^           ^Info^
^^^^^^^^-------------------------------------
_m_: mode             _p_: list packages
_k_: key-to-func      _a_: apropos
_K_: func-to-key      _M_: search emacs manual
_f_: function
_v_: variable         _d_: toggle error debugging
_c_: face             _w_: watch function for step-debugging
_b_: bindings (list)  _W_: stop watching function for step-debugging
_B_: bindings
"
    ("m" describe-mode)
    ("f" describe-function)
    ("v" describe-variable)
    ("k" describe-key)
    ("K" where-is)
    ("c" describe-face)
    ("b" counsel-descbinds)
    ("B" describe-bindings)
    ("p" package-list-packages)
    ("a" counsel-apropos)
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
    ("i" borg-assimilate "install")
    ("l" package-list-packages-no-fetch "package-list"))
  (defhydra hydra-submenu-project (:exit t)
    ("n" (let ((counsel-projectile-switch-project-action
                'counsel-projectile-switch-project-action-switch-to-buffer))
           (counsel-projectile-switch-project)) "buffer")
    ("m" (let ((counsel-projectile-switch-project-action
                'counsel-projectile-switch-project-action-fzf))
           (counsel-projectile-switch-project)) "file")
    ("," (let ((counsel-projectile-switch-project-action
                'counsel-projectile-switch-project-action-rg))
           (counsel-projectile-switch-project)) "contents")
    ("p" (let ((counsel-projectile-switch-project-action
                'counsel-projectile-switch-project-action-vc))
           (counsel-projectile-switch-project)) "vc")
    ("v" (let ((counsel-projectile-switch-project-action
                'counsel-projectile-switch-project-action-vc))
           (counsel-projectile-switch-project)) "vc")
    ("g" (let ((counsel-projectile-switch-project-action
                'counsel-projectile-switch-project-action-vc))
           (counsel-projectile-switch-project)) "vc"))
  (defhydra hydra-submenu-git (:exit t :hint nil)
    "
              ^Git^
^^^^^^^^---------------------------------
_g_: status      _L_: log       _b_: blame
_c_: clone       _f_: file log
_j_: smerge next _u_: upper     _e_: smerge
_k_: smerge prev _l_: lower     _m_: smerge
_P_: 80-char sentences
"
    ("g" magit-status)
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
  (defhydra hydra-submenu-org-mode (:exit t :hint nil)
    "
              ^org-mode^
^^^^^^^^-----------------------------------------------------------------------
 _A_: agenda  _c_: C-c C-c  _e_: encrypt entry        _s_: store link at P
 _b_: bnote   ^ ^           _E_: encrypt all entries  _S_: insert link at P
 _n_: narrow  ^ ^           _d_: decrypt entry        _o_: open link
 _N_: widen   ^ ^           _D_: decrypt all entries  _L_: toggle desc links
^ ^
 _m_: region->md  ^ ^       _t_: insert template      _l_: search & insert linked link
"
    ("A" org-agenda)
    ("b" duc/create-or-open-bnote)
    ("c" org-ctrl-c-ctrl-c)
    ("e" org-encrypt-entry)
    ("E" org-encrypt-entries)
    ("d" org-decrypt-entry)
    ("D" org-decrypt-entries)
    ("l" (duc/counsel-ag-insert-linked-link nil nil "--org" nil))
    ("L" org-toggle-link-display)
    ("n" org-narrow-to-subtree)
    ("N" widen)
    ("s" org-store-link)
    ("S" org-insert-link)
    ("m" org-md-convert-region-to-md)
    ("t" org-insert-structure-template)
    ("o" org-open-at-point))
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
    ("c" (counsel-rg nil "~/dev/notes/corpus"))
    ("d" osx-dictionary-search-word-at-point)
    ("i" org-download-screenshot)
    ("I" org-download-image)
    ("s" duc/forvo-text-to-sound-at-region-or-word))
  (defhydra hydra-main-menu (:exit t :idle .2 :hint nil)
    "
^Navigate^       ^Search^           ^Action^          ^Application
^^^^^^^^-----------------------------------------------------------------
_h_: left     _,_: in files       _SPC_: M-x          _g_: magit
_l_: right    _<_: occur in files   _b_: buffers      _o_: org-mode
_k_: up       ^ ^                   _e_: eval         _E_: eval-expression
_j_: down     _>_: occur in file    _w_: window/frame _s_: shell
_J_: jump     ^ ^                   _L_: lc           _u_: package
_\\_: vsplit   ^ ^                   ^ ^               _a_: anki
_-_: hsplit   ^ ^                   _H_: help
_n_: buffer   ^ ^                   _?_: help
_m_: files    ^ ^                   _f_: file
_p_: project  ^ ^                   _c_: customize
"
    ("h" evil-window-left)
    ("l" evil-window-right)
    ("k" evil-window-up)
    ("j" evil-window-down)
    ("J" ace-window)
    ("-" split-window-below)
    ("\\" split-window-right)
    ("," counsel-rg)
    ("<" deadgrep)
    (">" occur)
    ("?" hydra-submenu-help/body)
    ("n" switch-to-buffer)
    ("m" counsel-fzf)
    ("o" hydra-submenu-org-mode/body)
    ("p" hydra-submenu-project/body)
    ("s" duc/ivy-shell)
    ("SPC" execute-extended-command)
    ("b" hydra-submenu-buffer/body)
    ("c" hydra-submenu-customize-face/body)
    ("e" hydra-submenu-eval/body)
    ("E" eval-expression)
    ("w" hydra-submenu-window/body)
    ("H" hydra-submenu-help/body)
    ("f" hydra-submenu-file/body)
    ("g" hydra-submenu-git/body)
    ("u" hydra-submenu-package/body)
    ("L" hydra-submenu-leetcode/body)
    ("a" hydra-submenu-anki/body)))

(use-package ace-window
  :config
  ; aw-keys are 0-9 by default, which is reasonable, but in the setup above,
  ; the keys are on the home row.
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; themes

(use-package doom-modeline
  :disabled t
  :ensure t
  :after all-the-icons
  :init
  (setq doom-modeline-icon nil)
  :hook (after-init . doom-modeline-mode))

(use-package flatland-black-theme
  :no-require t)

(use-package seoul256-theme
  :config
  (setq seoul256-background 256))

(use-package doom-themes
  :after vterm
  :config
  (defun duc/theme-setup-doom-flatwhite-theme (&rest _)
    "Tweak vterm display colors for doom-flatwhite"
    (let ((current-theme (car custom-enabled-themes)))
      (when (eq current-theme 'doom-flatwhite)
        (set-face-attribute 'vterm-color-black nil
                            ;; "Normal" ansi color for foreground black (maybe).
                            :foreground "#7a7a7a"
                            ;; "Bright" ANSI color for foreground black (maybe).
                            :background "#a1a1a1"))))
  (advice-add 'load-theme :after #'duc/theme-setup-doom-flatwhite-theme))

(use-package solarized-theme)

;; end themes

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package rainbow-delimiters)

(use-package habamax-theme)

(use-package whitespace
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
                     javascript-mode) c c)))

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
     escape))
  (lispyville-mode))

(use-package ag)

(use-package deadgrep)

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

(use-package counsel-projectile
  :after projectile
  :init
  (defun counsel-projectile-switch-project-action-fzf (project)
    "Call `counsel-fzf' (ie fuzzy find-file)from PROJECT's root."
    (let ((default-directory project)
          (projectile-switch-project-action
           (lambda ()
             (counsel-fzf))))
      (counsel-projectile-switch-project-by-name project)))
  :config
  (counsel-projectile-mode))

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

(use-package ivy
  :after evil
  :bind (:map ivy-minibuffer-map ("M-x" . ivy-dispatching-done))
  :init
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-flx-limit 100)
  (setq ivy-re-builders-alist
        '((counsel-git-log . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (swiper-multi . ivy--regex-plus)
          (projectile-completing-read . ivy--regex-plus)
          (counsel-fzf . regexp-quote)
          (counsel-rg . ivy--regex-plus)
          (t . ivy--regex-plus)))
  (setq ivy-initial-inputs-alist nil)

  (setq ivy-count-format "")
  (setq ivy-height 15)
  ;; this is the default
  (setq ivy-do-completion-in-region t)
  :config
  ;; swapping behavior
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)

  (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-immediate-done)

  ;; Unbind ivy-restrict-to-matches to prevent clearing
  ;; minibuffer when chording S-SPC unintentionally.
  (define-key ivy-minibuffer-map (kbd "S-SPC") nil)

  ;; Escape quits.
  (with-eval-after-load 'evil
    (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)))

(use-package counsel
  :bind (("M-x" . counsel-M-x))
  :commands (counsel-ag
             counsel-find-file
             counsel-rg
             counsel-git
             counsel-fzf
             counsel-fzf-occur
             counsel-describe-face)
  :init
  (with-eval-after-load 'projectile
    (setq projectile-switch-project-action 'counsel-fzf))
  (setq counsel-async-filter-update-time 100000)

  (setq counsel-git-cmd "git ls-files --exclude-standard --full-name --others --cached --")
  (setq counsel-rg-base-command "rg -i --no-heading --line-number --color never %s .")
  (setq counsel-ag-base-command "ag -U --nocolor --nogroup %s -- .")
  :config
  (ivy-set-prompt 'counsel-fzf (lambda () "> "))
  (setenv "FZF_DEFAULT_COMMAND"
          "(git ls-files --exclude-standard --others --cached ||
        ind . -maxdepth 9 -path \"*/\\.*\" -prune -o -print -o -type l -print |
           sed s/^..//) 2> /dev/null"))

(use-package swiper
  :commands (swiper)
  :diminish ivy-mode
  :config
  ; Select input that happens to also match one of the candidates.
  ; e.g. Selecting 'bar' when there is candidate 'barricade'.
  ; Alternatively, c-M-j
  (setq ivy-use-selectable-prompt t))

(use-package selectrum
  :config
  (with-eval-after-load 'evil
    (define-key selectrum-minibuffer-map [escape] 'minibuffer-keyboard-quit))
  (selectrum-mode))

(use-package prescient
  :after counsel
  :init
  ;; Order of filter methods does not affect candidate order, but may affect
  ;; highlighting and query speed.
  (setq prescient-filter-method '(prefix initialism literal regexp))
  ;; Prescient does limited sorting of candidates by:
  ;; (1) How candidates are initially ordered as input.
  ;; (2) Candidate selection history.
  ;;   a. Last selected candidates.
  ;;   b. Followed by most frequently selected.
  ;; (3) Candidate string length.

  ;; Disable sorting by candidate length.
  (setq prescient-sort-length-enable nil)
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :after prescient
  ;; Configures prescient for sorting ivy candidates.
  :init
  (setq ivy-prescient-retain-classic-highlighting t)
  ;; AFAICT prescient does not affect ivy filtering configured by
  ;; `ivy-re-builders-alist`, so toggling this var does nothing.
  (setq ivy-prescient-enable-filtering nil)
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode))

(use-package selectrum-prescient
  :after selectrum
  :config
  (selectrum-prescient-mode))

(defun disable-company-mode-in-eshell-mode ()
  (company-mode -1))

(use-package company
  :diminish company-mode
  :init
  (setq company-idle-delay .01)
  (setq company-minimum-prefix-length 1)
  :config
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

(use-package flycheck)

(use-package tide
  :commands (tide-setup)
  :config
  (setq tide-jump-to-definition-reuse-window nil)
  ;; Set up Typescript linting with `web-mode'.
  ;; https://github.com/ananthakumaran/tide/pull/161
  (eval-after-load 'flycheck
    (lambda ()
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

  (add-hook 'web-mode-hook
            (lambda ()
              ;; Set up Tide mode if Typescript.
              (when (string-equal (file-name-extension buffer-file-name) "tsx")
                (setq-local web-mode-enable-auto-quoting nil)
                (when (fboundp 'yas-activate-extra-mode)
                  (yas-activate-extra-mode 'typescript-mode))
                (+setup-tide-mode)))))

(use-package restclient)

(use-package magit
  :commands (magit-toplevel
             magit-status
             magit-blame
             magit-log
             magit-find-file
             magit-find-file-other-window)
  :init
  (defun +magit-git-submodule-update--init--recursive ()
    "Run $ git submodule update --init --recursive."
    (interactive)
    (magit-run-git-async "submodule" "update" "--init" "--recursive"))

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

  (define-key magit-hunk-section-map (kbd "<return>") 'magit-diff-visit-file-other-window)

  ;; Bind esc
  (define-key transient-map        (kbd "<escape>") 'transient-quit-one)
  (define-key transient-edit-map   (kbd "<escape>") 'transient-quit-one)
  (define-key transient-sticky-map (kbd "<escape>") 'transient-quit-seq))

(use-package evil-ediff
  :commands (evil-ediff-init)
  :init
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options "-w")
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  (defun +evil-ediff-init ()
    "Initialize with `evil-ediff-init' and remove the hook."
    (evil-ediff-init)
    (remove-hook 'ediff-mode-hook #'evil-ediff-init))
  (add-hook 'ediff-mode-hook #'+evil-ediff-init))

(use-package ibuffer-sidebar
  :init
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face '(:family duc/font-family :height 120)))

(use-package dired-subtree
  :commands (dired-subtree-toggle dired-subtree-cycle)
  :init
  (setq dired-subtree-use-backgrounds nil))

(use-package vscode-icon)

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

(use-package swift-mode)

(use-package kotlin-mode)

(use-package lsp-mode
  :init
  (setq lsp-log-io t))

; Python projects should initialize the python lsp themselves.
; using
;   (lsp) after require
(use-package lsp-python-ms
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms))))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package racket-mode)

(use-package rainbow-mode)

(use-package vimrc-mode)

(use-package restclient)

; Remove .json from using major mode
; Fixes issue where loading large json file freezes emacs.
(setq auto-mode-alist (rassq-delete-all 'javascript-mode auto-mode-alist))

(use-package vterm
  :if (not (eq system-type 'windows-nt))
  :after general
  :init
  (defvar vterm-install t)
  (setq vterm-module-cmake-args "-D USE_SYSTEM_LIBVTERM=no")
  (setq vterm-kill-buffer-on-exit nil)
  (setq vterm-max-scrollback 100000)
  (setq vterm-clear-scrollback-when-clearing t)
  :config
  (general-define-key
   :keymaps 'vterm-mode-map
   "M-<escape>" 'evil-collection-vterm-toggle-send-escape)
  (general-define-key
   :keymaps 'vterm-mode-map
   "M-k" 'vterm-clear))

(use-package tex
  :defer t
  :init
  (setq TeX-engine "xelatex")
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

(use-package leetcode
  :init
  (setq leetcode-prefer-language "python3")
  :config
  (evil-define-key 'normal tabulated-list-mode-map (kbd "RET") 'leetcode-show-current-problem))

(use-package ereader
  :mode
  ("\\.epub\\'" . ereader-mode)
  :config
  (evil-define-key 'normal ereader-mode-map (kbd "0") 'evil-digit-argument-or-evil-beginning-of-line))

(use-package org-download
  :init
  (setq org-download-image-dir "~/dev/notes/img")
  (setq org-download-screenshot-method "screencapture -i %s"))

(use-package anki-editor
  :init
  (setq anki-editor-org-tags-as-anki-tags nil)
  (setq request-log-level 'debug))

(use-package osx-dictionary)

(use-package elfeed
  :config
  (setq elfeed-feeds local/elfeed-feeds))


;; End package declarations

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(fci-rule-color "#171717")
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   '("#3b6b40f432d6" "#07b9463c4d36" "#47a3341e358a" "#1d873c3f56d5" "#2d86441c3361" "#43b7362d3199" "#061d417f59d7"))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#5b7300" . 20)
     ("#007d76" . 30)
     ("#0061a8" . 50)
     ("#866300" . 60)
     ("#992700" . 70)
     ("#a00559" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#866300" "#992700" "#a7020a" "#a00559" "#243e9b" "#0061a8" "#007d76" "#5b7300"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(lsp-ui-doc-border "#93a1a1")
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(org-agenda-files '("~/dev/notes/log.org"))
 '(org-babel-load-languages '((emacs-lisp . t) (R . t) (ditaa . t) (plantuml . t)))
 '(org-confirm-babel-evaluate nil)
 '(package-selected-packages
   '(spacegray-theme darkmine-theme oceanic-theme soft-morning-theme grandshell-theme zweilight-theme punpun-theme badger-theme omtose-phellack-theme busybee-theme phoenix-dark-pink-theme phoenix-dark-mono-theme inkpot-theme panda-theme ujelly-theme night-owl-theme gruvbox-theme underwater-theme madhat2r-theme darkburn-theme northcode-theme zerodark-theme nord nord-theme fold-dwim-org origami outshine esh-autosuggest go-mode ibuffer-vc ibuffer-projectile counsel-projectile counsel-tramp doom-modeline jazz-theme jbeans-theme klere-theme kooten-theme lenlen-theme mbo70s-theme melancholy-theme mellow-theme metalheart-theme mustang-theme solarized-theme sunburn-theme blackboard-theme bliss-theme bubbleberry-theme danneskjold-theme firecode-theme farmhouse-theme eziam-theme ibuffer-sidebar seoul257-theme twilight-bright-theme labburn-theme moe-theme borland-blue-theme autumn-light-theme switch-window restclient moom pkg one-themes ones-theme doneburn-theme plain-theme iodine-theme nofrils-acme-theme nofrils-acme groovy-mode gradle-mode rainbow-blocks rainbow-mode challenger-deep-theme kosmos-theme cosmos-theme habamax-theme kaolin-themes swift3-mode nimbus-theme hydandata-light-theme monotropic-theme darkokai-theme cyberpunk-theme objc-font-lock base16-themes base16 swift-mode darktooth-theme kotlin-mode csharp-mode doom hemisu-theme material-theme flatland-theme light-soap-theme yoshi-theme sexy-monochrome-theme paper-theme hc-zenburn-theme sourcerer-theme github-modern-theme green-is-the-new-black-theme greymatters-theme eclipse-theme distinguished-theme dark-mint-theme dakrone-light-theme cherry-blossom-theme atom-one-dark-theme atom-dark-theme ahungry-theme color-theme-approximate graphene-meta-theme spacemacs-theme elogcat which-key plan9-theme tao-theme eink-theme inverse-acme-theme gruber-darker-theme flatui-dark-theme flatui-theme leuven-theme creamsody-theme apropospriate-theme highlight-indent-guides evil-collection anti-zenburn zenburn markdown-mode sublimity-map sublimity diff-hl macrostep zenburn-theme anti-zenburn-theme minimap doom-themes dracula-theme projectile lispyville smartparens diminish evil-magit company multi-term magit all-the-icons-dired dired-sidebar dired-subtree tide web-mode exec-path-from-shell typescript-mode company-mode counsel ivy rainbow-delimiters hydra evil ht log4e dash))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(safe-local-variable-values
   '((eval lsp)
     (eval set
           (make-local-variable 'lsp-python-ms-python-executable-cmd)
           (concat
            (projectile-project-root)
            ".venv/bin/python"))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(vc-annotate-background "#0E0E0E")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#616161")
     (40 . "#9E9E9E")
     (60 . "#9E9E9E")
     (80 . "#C3C3C3")
     (100 . "#C3C3C3")
     (120 . "#DADADA")
     (140 . "#DADADA")
     (160 . "#E8E8E8")
     (180 . "#E8E8E8")
     (200 . "#E8E8E8")
     (220 . "#F1F1F1")
     (240 . "#F1F1F1")
     (260 . "#F1F1F1")
     (280 . "#F6F6F6")
     (300 . "#F6F6F6")
     (320 . "#F6F6F6")
     (340 . "#FAFAFA")
     (360 . "#FAFAFA")))
 '(vc-annotate-very-old-color "#DADADA")
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#839496" "#657b83")))
