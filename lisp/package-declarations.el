;;;; -*- lexical-binding: t; -*-
;; package management
;;

(use-package bind-key
  :ensure t)

(use-package duc
  :straight nil
  :init
  ; e.g., switch-to-buffer respects other-window-prefix
  (setq switch-to-buffer-obey-display-actions t)
  (setq display-buffer-alist
        '(("\.org$"
           (display-buffer-reuse-window
            display-buffer-below-selected)
           (inhibit-same-window . t)
           (window-min-width . 50)
           (window-min-height . 20))
          ("terminal-epijudge"
           ()
           (inhibit-same-window . t))))
  :config

  ;; Set font
  (set-face-attribute 'default nil
                      :family duc/font-family
                      :height duc/font-height
                      :weight duc/font-weight
                      :width 'normal)

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
              (auto-fill-mode t)
              (setq-local truncate-lines t)))

  (add-hook 'before-save-hook
            (lambda ()
              (pcase major-mode
                ('org-mode (if (s-ends-with-p "/dev/notes/" (file-name-directory (buffer-file-name)))
                               (duc/bnote-update-backlinks-for-note)))))))

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

  (transient-define-prefix leader-main-menu ()
    "Main"
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
      ("," "in files" counsel-rg)
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
      ("u" "package" hydra-submenu-package/body)
      ("A" "anki" hydra-submenu-anki/body)
      ("R" "org-fc" transient-org-fc)]]
    [["More Navigation"
      ("n" "buffer" switch-to-buffer)
      ("m" "files" projectile-find-file-dwim)
      ("M" "files (all)" project-or-external-find-file)
      ("p" "project" hydra-submenu-project/body)]
     ["Other"
      ("H" "help" hydra-submenu-help/body)
      ("?" "help" hydra-submenu-help/body)
      ("f" "file" transient-file)
      ("c" "customize" hydra-submenu-customize-face/body)]]
    (interactive)
    (let ((transient-show-popup -0.2))
      (transient-setup 'leader-main-menu))))

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
    ("c" (counsel-rg nil "~/dev/notes/corpus"))
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
  :straight (:type built-in)
  :config
  (setq save-abbrevs 'silent) ;; save abbrevs when files are saved
  :init
  (setq abbrev-file-name
        (expand-file-name "abbrev_defs.el" user-emacs-directory)))

(use-package tree-sitter)

(use-package tree-sitter-langs
  :after tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; themes

(use-package flatland-black-theme
  :no-require t)

(use-package seoul256-theme
  :straight (:host github
                   :repo "dangduc/seoul256-emacs")
  :no-require t
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

(use-package catppuccin-theme)

(use-package modus-themes
  :init
  (setq modus-themes-deuteranopia t)

  (defun duc/theme-setup-modus-vivendi-theme (&rest _)
    "Tweak vterm display colors for doom-flatwhite"
    (let ((current-theme (car custom-enabled-themes)))
      (when (eq current-theme 'modus-vivendi)
        set-face-attribute 'modus-themes-hl-line nil :background "#dedede")))
  (advice-add 'load-theme :after #'duc/theme-setup-modus-vivendi-theme)

  (defun duc/theme-setup-modus-operandi-theme (&rest _)
    "Tweak vterm display colors for doom-flatwhite"
    (let ((current-theme (car custom-enabled-themes)))
      (when (eq current-theme 'modus-operandi)
        (set-face-attribute 'modus-themes-hl-line nil :background "#DEECF4"))))
  (advice-add 'load-theme :after #'duc/theme-setup-modus-operandi-theme))

(use-package mindre-theme
  :init
  (setq mindre-use-more-bold nil)
  (setq mindre-use-faded-lisp-parens t))

;; end themes

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package rainbow-delimiters
  :straight (:host github
             :repo "Fanael/rainbow-delimiters"))

(use-package habamax-theme
  :no-require t)

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
     atom-movement
     prettify
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

(use-package vertico
  :straight (vertico
             :includes
             (
              ;; vertico-buffer
              vertico-directory
              ;; vertico-flat
              ;; vertico-grid
              ;; vertico-indexed
              ;; vertico-mouse
              ;; vertico-multiform
              ;; vertico-quick
              ;; vertico-repeat
              ;; vertico-reverse
              ;; vertico-unobtrusive
              )
             :files (:defaults "extensions/*"))
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :ensure t
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package orderless
  :straight t
  :ensure t
  :commands (orderless-filter))

(use-package flx-rs
  :ensure t
  :straight
  (flx-rs
   :repo "jcs-elpa/flx-rs"
   :fetcher github
   :files (:defaults "bin"))
  :config
  (flx-rs-load-dyn)
  ;; This is not necessary since `flx-all-completions' already checks for this
  ;; function. It'll still help other libraries that call `flx-score' though.
  (advice-add 'flx-score :override #'flx-rs-score))

(use-package flx)

(use-package orderless
  :straight t
  :ensure t
  :commands (orderless-filter))

(use-package flx-rs
  :ensure t
  :straight
  (flx-rs
   :repo "jcs-elpa/flx-rs"
   :fetcher github
   :files (:defaults "bin"))
  :config
  (flx-rs-load-dyn))

(use-package flx)

(use-package fzf-native
  :straight
  (:repo "dangduc/fzf-native"
   :host github
   :files (:defaults "bin" "*.c" "*.h" "*.txt"))
  :init
  (setq fzf-native-always-compile-module t)
  :config
  (fzf-native-load-own-build-dyn))

(use-package fussy
  :ensure t
  :straight
  (fussy :type git :host github :repo "jojojames/fussy")
  :after flx
  :config
  (setq fussy-score-fn 'fussy-fzf-native-score)
  (setq fussy-filter-fn 'fussy-filter-flex)

  (push 'fussy completion-styles)
  (setq
   ;; For example, project-find-file uses 'project-files which uses
   ;; substring completion by default. Set to nil to make sure it's using
   ;; flx.
   completion-category-defaults nil
   completion-category-overrides nil)

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
  (setq company-idle-delay 0.01)
  (setq company-minimum-prefix-length 1)
  :config
  (defun d-company-capf-with-og-completion-styles (f &rest args)
    "Set `completion-styles' to be the default Emacs `completion-styles'
while `company-capf' runs."
    (let ((completion-styles '(basic substring flx)))
      (apply f args)))
  (advice-add 'company-capf :around 'd-company-capf-with-og-completion-styles)

  (push 'duc/company-shortcut company-backends)

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

(use-package flycheck
  :init
  (add-hook 'sh-mode-hook (lambda () (flycheck-mode 1)))
  (add-hook 'tide-mode-hook (lambda () (flycheck-mode 1))))

(use-package tide
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

(use-package restclient)

(use-package transient
  :config
  ;; Bind esc
  (define-key transient-map (kbd "<escape>") 'transient-quit-all)
  (define-key transient-edit-map (kbd "<escape>") 'transient-quit-one)
  (define-key transient-sticky-map (kbd "<escape>") 'transient-quit-seq)
  (define-key transient-map (kbd "q") 'transient-quit-all)
  (define-key transient-edit-map (kbd "q") 'transient-quit-one)
  (define-key transient-sticky-map (kbd "q") 'transient-quit-seq)

  (defmacro transient-define-suffix--transient-buffer (&rest commands)
    "Create transient suffixes for transient-buffer that can recognize args"
    `(progn
       ,@(cl-loop
          for command in commands
          appending
          (let* ((str (symbol-name command))
                 (sym (intern (format "transient-%s--args" (symbol-name command)))))
            `((transient-define-suffix ,sym (&optional args)
                (concat ,str " that recognizes transient args")
                (interactive (list (transient-args transient-current-command)))
                (transient-set)
                (let ((other-window-prefix-switch
                       (transient-arg-value "--other-window-prefix" args)))
                  (when other-window-prefix-switch
                    (other-window-prefix))
                  (call-interactively (quote ,command)))))))))

  (transient-define-suffix--transient-buffer switch-to-buffer
                                             duc/new-buffer
                                             list-buffers)

  (transient-define-prefix transient-buffer ()
    "buffer"
    ["Switches"
     ("w" "other-window-prefix" "--other-window-prefix")]
    [["edit"
      ("N" "new" transient-duc/new-buffer--args)
      ("m" "move buffer & file (ie, rename)" (lambda () (interactive) (duc/rename-file (buffer-name))))
      ("r" "rename" rename-buffer)
      ("R" "reload" revert-buffer)
      ("k" "kill buffer" kill-buffer)]
     ["navigation"
      ("p" "prev" previous-buffer)
      ("n" "next" next-buffer)
      ("l" "list buffers" transient-list-buffers--args)
      ("o" "switch" transient-switch-to-buffer--args)]
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
      ('pdf-view-mode (transient-major-pdf-view))))
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
      ("l" "search & insert link" (lambda () (interactive)
                                    (duc/counsel-ag-insert-linked-link nil nil "--org" nil)))
      ("s" "store link at P" org-store-link)
      ("S" "insert link at P" org-insert-link)]
     ["view"
      ("A" "agenda" org-agenda)
      ("L" "toggle descriptive links" org-toggle-link-display)
      ("i" "toggle inline images" org-toggle-inline-images)
      ("n" "narrow" org-narrow-to-subtree)
      ("N" "widen" widen)
      ("w" "widen" widen)
      ("o" "open link" org-open-at-point)]])
  (transient-define-prefix transient-org-fc ()
    "org-fc"
    [["Review"
      ("RET" "flip" org-fc-review-flip)
      ("p" "edit" org-fc-review-edit)
      ("i" "edit" org-fc-review-edit)
      ("q" "quit" org-fc-review-quit)
      ("s" "suspend" org-fc-review-suspend-card)]
     ["Rate"
      ("a" "again" org-fc-review-rate-again)
      ("e" "easy" org-fc-review-rate-easy)
      ("g" "good" org-fc-review-rate-good)
      ("h" "hard" org-fc-review-rate-hard)
      ("1" "easy" org-fc-review-rate-easy)
      ("2" "good" org-fc-review-rate-good)
      ("3" "hard" org-fc-review-rate-hard)]
     ["Capture"
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

(use-package magit
  :straight (:build (:not compile)) ;; https://github.com/magit/magit/issues/4676
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
  ; provided by dired-hacks.
  :commands (dired-subtree-toggle dired-subtree-cycle)
  :init
  (setq dired-subtree-use-backgrounds nil))

(use-package vscode-icon
  :straight (:host github
             :repo "jojojames/vscode-icon-emacs"
             :files (:defaults "icons" "source")))

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
  (setq lsp-log-io t)
  (setq lsp-lens-auto-enable nil)
  :hook ((c-mode . lsp)
         (clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)))

(use-package ccls)

; Python projects should initialize the python lsp themselves.
; using
;   (lsp) after require
(use-package lsp-python-ms
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms))))

(setq python-indent-offset 2)

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
  :straight auctex
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
  :init
  (setq leetcode-prefer-language "python3")
  :config
  (evil-define-key 'normal tabulated-list-mode-map (kbd "RET") 'leetcode-show-current-problem))

(use-package ereader
  :mode
  ("\\.epub\\'" . ereader-mode)
  :config
  (evil-define-key 'normal ereader-mode-map (kbd "0") 'evil-digit-argument-or-evil-beginning-of-line))

(use-package ob-asymptote
  :after org
  :straight (:type git
                   :repo "https://git.sr.ht/~bzg/org-contrib"
                   :files ("lisp/ob-asymptote.el"))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((asymptote . t))))

(use-package asy-mode
  :after org-contrib
  :straight (:host github
                   :repo "vectorgraphics/asymptote"
                   :files (:defaults "base/asy-mode.el")))

(use-package org
  :straight (:type built-in)
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

  (setq org-default-notes-file "~/dev/notes/log.org")
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
  (setq org-edit-src-content-indentation 0))

(use-package org-roam
  :init
  (let ((d "~/dev/rotes"))
    (unless (file-exists-p d)
      (make-directory d))
    (setq org-roam-directory (file-truename d)))
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
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

(use-package org-ql)

(use-package org-fc
  :straight
  (org-fc :type git
          :repo "https://git.sr.ht/~l3kn/org-fc"
          :files (:defaults "awk" "demo.org"))
  :init
  (let ((dir "~/dev/org-fc"))
    (unless (file-exists-p dir)
      (make-directory dir))
    (setq org-fc-directories `(,dir))
    (setq org-fc-review-history-file (expand-file-name "org-fc-reviews.tsv" dir)))
  :config
  (set-face-attribute 'org-fc-type-cloze-hole-face nil :foreground "blue"
                                                       :weight 'unspecified)
  (require 'org-fc-hydra))

(use-package anki-editor
  :init
  (setq anki-editor-org-tags-as-anki-tags nil)
  (setq request-log-level 'debug))

(use-package osx-dictionary)

(use-package elfeed
  :config
  (setq elfeed-feeds local/elfeed-feeds))

(use-package geiser
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

(use-package clojure-mode)

(use-package geiser-mit
  :after geiser)

(use-package lua-mode)

(use-package pdf-tools
  :init
  (setq pdf-view-resize-factor 1.1)
  (setq-default pdf-view-display-size 'fit-page)
  :config
  (pdf-tools-install))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link)
  :config
  (defun duc/yank-org-pdftools-get-link ()
    "Use while pdf text highlighted to yank org-link to text, e.g.,
[[pdf:~/dev/pdfs/some-paper.pdf::16++16.23;;annot-16-0]]"
    (interactive)
    (duc/yank-string (org-pdftools-get-link))))

(provide 'package-declarations)
