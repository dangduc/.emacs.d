(load "~/.emacs.d/duc.el")

(setq ring-bell-function #'ignore)

(setq gc-cons-threshold 100000000) ; 100 mb

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

;; don't recenter when scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; display columns position in modeline
(column-number-mode t)

;; enable transparent osx titlebar (a la Chrome)
(duc/alist-replace-set default-frame-alist (ns-transparent-titlebar . t))

;; nil or dark, to switch to between black or white title text
;(duc/alist-replace-set default-frame-alist (ns-appearance . dark|nil))
(duc/alist-replace-set default-frame-alist (ns-appearance . nil))

;(duc/alist-replace-set default-frame-alist (ns-use-thin-smoothing . t))
(duc/alist-replace-set default-frame-alist (ns-antialias-text . nil))

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

;; Set font
(set-face-attribute 'default nil
                    :family "InconsolateG for Powerline"
                    :height duc/font-height
                    :weight 'normal
                    :width 'normal)

(advice-add 'load-theme :after #'duc/theme-setup-modeline)

(advice-add 'load-theme :after #'duc/org-mode-theme)

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

;; package management
;;

;; Disable in favor of `use-package'.
(setq package-enable-at-startup nil)

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Specifying :straight t is unnecessary if you set straight-use-package-by-default to a non-nil value.
(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

;; end bootstrap straight.el

;; Package declarations
;;

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
  :config
  (setq evil-want-C-u-scroll t)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  ; " make j, k move cursor screen-wise, not line-wise. Makes word-wrapped
  ; " paragraph navigation sane. (http://statico.github.com/vim.html)
  ; :nmap j gj
  ; :nmap k gk
  ;                                      ;
  ; [How to map j and k to gj and gk in Emacs Evil-mode?](https://stackoverflow.com/questions/23576163/)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  ; " scroll by N lines instead of default 1 line.
  ; set scrolloff=3
  (setq scroll-margin 3)
  :init
  (setq evil-want-integration nil)
  (evil-mode 1))

(use-package evil-collection
    :after evil
    :init
    (evil-collection-init))

(use-package general
  :after evil
  :config
  (general-override-mode)
  (general-define-key
   :states '(normal motion visual)
   :keymaps 'override
   "<SPC>" 'hydra-main-menu/body))

(use-package hydra
  :config
  (defhydra hydra-submenu-buffer (:exit t)
    ("p" previous-buffer "prev buffer")
    ("n" next-buffer "next buffer")
    ("l" list-buffers "list buffers")
    ("N" duc/new-buffer "new buffer")
    ("c" duc/new-buffer "new buffer")
    ("o" switch-to-buffer "switch buffer")
    ("r" revert-buffer "reload buffer")
    ("w" save-buffer "save buffer")
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
  _c_: close
  _
"
    ("f" toggle-frame-maximized)
    ("w" toggle-frame-maximized)
    ("n" other-frame)
    ("N" make-frame-command)
    ("c" close-frame)
    ("b" balance-windows)
    ("k" delete-window))
  (defhydra hydra-submenu-file (:exit t)
    ("f" find-file "find file")
    ("w" save-buffer "write file")
    ("i" (find-file "~/.emacs.d/init.el" ) "init.el")
    ("d" (find-file "~/.emacs.d/duc.el" ) "duc.el")
    ("b" duc/sidebar-toggle "sidebar"))
  (defhydra hydra-submenu-help (:exit t :hint nil)
    "
^Describe^           ^Info^
^^^^^^^^-------------------------------------
_m_: mode            _p_: list packages
_k_: key             _a_: apropos
_f_: function
_v_: variable        _d_: toggle error debugging
_c_: face
"
    ("m" describe-mode)
    ("f" counsel-describe-function)
    ("v" counsel-describe-variable)
    ("k" describe-key)
    ("c" describe-face)
    ("p" package-list-packages)
    ("a" counsel-apropos)
    ("d" toggle-debug-on-error))
  (defhydra hydra-submenu-customize-face (:exit t :hint nil)
    "
^Font^                  ^Face^                 ^Buffer^
^^^^^^^^----------------------------------------------------------------
_f_: font            _c_: describe face     _r_: hex colors
_s_: font size       _t_: theme             _w_: whitespace
_+_: text scale +    ^ ^                    _l_: word-wrap
_-_: text scale -
_=_: text scale =
"
    ("f" duc/ivy-font)
    ("s" duc/font-size)
    ("+" text-scale-increase :color red)
    ("-" text-scale-decrease :color red)
    ("=" (text-scale-mode -1) :color red)
    ("c" counsel-describe-face)
    ("t" counsel-load-theme)
    ("r" rainbow-mode)
    ("w" whitespace-mode)
    ("l" visual-line-mode))
  (defhydra hydra-submenu-package (:exit t)
    ("i" straight-use-package "install")
    ("f" straight-freeze-versions "freeze lockfile")
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
^ ^              _f_: file log
_j_: smerge next _u_: upper     _e_: smerge
_k_: smerge prev _l_: lower     _m_: smerge

_P_: 80-char sentences
"
    ("g" magit-status)
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
  (defhydra hydra-submenu-org-mode (:exit t)
    ("c" org-ctrl-c-ctrl-c "ctrl-c-ctrl-c"))
  (defhydra hydra-main-menu (:exit t :idle .2 :hint nil)
    "
^Window^       ^Search^           ^Action^          ^Application
^^^^^^^^-----------------------------------------------------------------
_h_: left      _,_: contents    _SPC_: M-x          _g_: magit
_l_: right     _n_: buffers       _b_: buffers      _o_: org-mode
_k_: up        _m_: files         _e_: eval         _s_: shell
_j_: down      _p_: projects      _w_: window/frame _u_: package
_a_: jump      _<_: ls list       ^ ^
_\\_: vsplit  ^ ^                 ^ ^
_-_: hsplit    ^ ^                _?_: help
^^             ^ ^                _f_: file
^^             ^ ^                ^ ^
^ ^            ^ ^                _c_: customize
"
    ("h" evil-window-left)
    ("l" evil-window-right)
    ("k" evil-window-up)
    ("j" evil-window-down)
    ("a" ace-window)
    ("-" split-window-below)
    ("\\" split-window-right)
    ("," counsel-rg)
    ("<" projectile-ag)
    ("n" switch-to-buffer)
    ("m" counsel-fzf)
    ("o" hydra-submenu-org-mode/body)
    ("p" hydra-submenu-project/body)
    ("s" duc/ivy-shell)
    ("SPC" execute-extended-command)
    ("b" hydra-submenu-buffer/body)
    ("c" hydra-submenu-customize-face/body)
    ("e" hydra-submenu-eval/body)
    ("w" hydra-submenu-window/body)
    ("?" hydra-submenu-help/body)
    ("f" hydra-submenu-file/body)
    ("g" hydra-submenu-git/body)
    ("u" hydra-submenu-package/body)))

(use-package ace-window
  :init
  ; aw-keys are 0-9 by default, which is reasonable, but in the setup above,
  ; the keys are on the home row.
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; themes

(advice-add 'load-theme :after 'duc/org-mode-theme)

(use-package zenburn-theme
  :disabled
  :init
  (load-theme 'zenburn t)
  (set-face-attribute 'fringe nil :background nil)
  (set-face-attribute 'vertical-border nil :foreground (face-attribute 'mode-line :background)))

(use-package doom-themes)

(use-package solarized-theme)

(use-package nofrils-acme-theme
  :no-require t)

(use-package tao-theme
  :no-require t)

(use-package nord-theme
  :no-require t
  :config
  (setq nord-region-highlight "snowstorm")
  (setq nord-comment-brightness 15))

(use-package zerodark-theme
  :no-require t)

(use-package commentary-theme
  :no-require t
  :straight (:host github
                   :repo "pzel/commentary-theme"))

(use-package parchment-theme
  :no-require t
  :straight (:host github
                   :repo "ajgrf/parchment"))

(use-package seoul256-theme
  :no-require t
  :config
  (setq seoul256-background 253))

(use-package habamax-theme
  :no-require t)

(use-package commentary-theme
  :no-require t
  :straight (:host github
                   :repo "pzel/commentary-theme"))

(use-package default-black-theme
  :no-require t
  :straight (:host github
             :repo "dangduc/default-black-theme"))

;; end themes

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package rainbow-delimiters
  :straight (:host github
             :repo "Fanael/rainbow-delimiters"))

(use-package whitespace
  :diminish whitespace-mode
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face lines-tail
                                tabs
                                trailing
                                empty
                                space-before-tab::tab
                                space-before-tab::space))
  :init
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
                  emacs-lisp-mode-hook)))
    ;(add-hook hook #'smartparens-strict-mode)
  :config
  ;; Disable highlights.
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (setq sp-cancel-autoskip-on-backward-movement nil
        sp-autoskip-closing-pair 'always-end
        sp-autoskip-opening-pair t)

  ;(use-package smartparens-config)
  (smartparens-global-mode 1)
  (sp-pair "(" ")" :wrap "M-(")
  (sp-pair "(" ")" :wrap "M-)")
  (sp-pair "[" "]" :wrap "M-[")
  (sp-pair "[" "]" :wrap "M-]")
  (sp-pair "{" "}" :wrap "M-{")
  (sp-pair "{" "}" :wrap "M-}")
  (sp-pair "\"" "\"" :wrap "M-\""))

(use-package lispyville
  :diminish (lispyville-mode)
  :commands
  (lispyville-mode)
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
     ;(additional-movement normal)
     slurp/barf-cp
     additional
     escape)))

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
  :config
  (setq projectile-enable-caching t)
  (projectile-mode))

(use-package counsel-projectile
  :after projectile
  :init
  (counsel-projectile-mode)
  (defun counsel-projectile-switch-project-action-fzf (project)
    "Call `counsel-fzf' (ie fuzzy find-file)from PROJECT's root."
    (let ((default-directory project)
          (projectile-switch-project-action
           (lambda ()
             (counsel-fzf))))
      (counsel-projectile-switch-project-by-name project))))

(use-package ibuffer-projectile
  :commands (ibuffer-projectile-set-filter-groups
             ibuffer-projectile-generate-filter-groups)
  :init
  (defun +ibuffer-projectile-run ()
    "Set up `ibuffer-projectile'."
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))

  (add-hook 'ibuffer-sidebar-mode-hook #'+ibuffer-projectile-run)
  (add-hook 'ibuffer-hook #'+ibuffer-projectile-run)
  :config
  (setq ibuffer-projectile-prefix "Project: "))

(use-package rainbow-delimiters
  :config (setq show-paren-delay 0)
  (show-paren-mode 1)
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ivy
  :bind (:map ivy-minibuffer-map
              ("M-x" . ivy-dispatching-done))
  :config
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-flx-limit 100)
  (setq ivy-re-builders-alist
        '((counsel-git-log . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (swiper-multi . ivy--regex-plus)
          (projectile-completing-read . ivy--regex-fuzzy)
          (counsel-fzf . regexp-quote)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)

  ;; swapping behavior
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)

  (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-immediate-done)

  ;; Escape quits.
  (with-eval-after-load 'evil
    (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit))

  (setq ivy-count-format "")
  (setq ivy-height 15)

  (setq ivy-do-completion-in-region t) ; this is the default

  (ivy-mode))

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
  (setq projectile-switch-project-action 'counsel-fzf)
  :config
  (ivy-set-prompt 'counsel-fzf (lambda () "> "))
  (setenv "FZF_DEFAULT_COMMAND"
          "(git ls-files --exclude-standard --others --cached ||
        ind . -maxdepth 9 -path \"*/\\.*\" -prune -o -print -o -type l -print |
           sed s/^..//) 2> /dev/null")
  (setq counsel-async-filter-update-time 100000)
  (setq counsel-git-cmd "git ls-files --exclude-standard --full-name --others --cached --")
  (setq counsel-rg-base-command "rg -i --no-heading --line-number --color never %s .")
  (setq counsel-ag-base-command "ag -U --nocolor --nogroup %s -- ."))

(use-package swiper
  :commands (swiper)
  :diminish ivy-mode)

(use-package flycheck
  :if (not (eq system-type 'windows-nt))
  :defer 8
  :diminish flycheck-mode
  :commands (flycheck-mode)
  :config
  (setq flycheck-idle-change-delay 1.2)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (global-flycheck-mode))

(defun disable-company-mode-in-eshell-mode ()
  (company-mode -1))

(use-package company
  :diminish company-mode
  :config
  (company-tng-configure-default)
  (setq company-idle-delay .01)
  (setq company-minimum-prefix-length 1)
  (global-company-mode))
  ;:hook (eshell-mode . disable-company-mode-in-eshell-mode))

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
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              ;; Set up indentation.
              (let ((n 2))
                (setq-local web-mode-markup-indent-offset n)
                (setq-local web-mode-css-indent-offset n)
                (setq-local web-mode-code-indent-offset n)
                (with-eval-after-load 'evil
                  (setq-local evil-shift-width n)))))
  :config
  ;; Use `company-dabbrev-code' with `web-mode'.
  (when (boundp 'company-dabbrev-code-modes)
    (push 'web-mode company-dabbrev-code-modes))

  (with-eval-after-load 'evil
    (evil-define-key 'normal web-mode-map
      (kbd "C-d") 'evil-scroll-down)))

(use-package typescript-mode
  ;; npm install -g typescript
  :mode
  ("\\.ts\\'" . typescript-mode)
  ("\\.ts$\\'" . typescript-mode)
  :init
  (add-hook 'typescript-mode-hook
            (lambda ()
              (setq-local typescript-indent-level 2)
              (with-eval-after-load 'evil
                (setq-local evil-shift-width typescript-indent-level))))
  :config
  (setq typescript-enabled-frameworks '(typescript)))

(use-package tide
  :commands (tide-setup)
  :init
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
                (+setup-tide-mode))))
  :config
  ;; Set up Typescript linting with `web-mode'.
  ;; https://github.com/ananthakumaran/tide/pull/161
  (eval-after-load 'flycheck
    (lambda ()
      (flycheck-add-mode 'typescript-tslint 'web-mode))))

(use-package magit
  :commands (magit-toplevel
             magit-status
             magit-blame
             magit-log
             magit-find-file
             magit-find-file-other-window)
  :config
  (defun +magit-git-submodule-update--init--recursive ()
    "Run $ git submodule update --init --recursive."
    (interactive)
    (magit-run-git-async "submodule" "update" "--init" "--recursive"))

  (magit-define-popup-action
   'magit-submodule-popup ?U
   "Update Init Recursive"
   #'+magit-git-submodule-update--init--recursive)

  (setq magit-bury-buffer-function 'magit-mode-quit-window)

  (setq magit-diff-refine-hunk 'all)
  ;(setq magit-diff-arguments '("--no-ext-diff" "--stat" "-U5"))

  ;; Save buffers automatically instead of asking.
  (setq magit-save-repository-buffers 'dontask)

  (setq magit-repository-directories '("~/dev" "~/.emacs.d"))
  (setq magit-refresh-status-buffer nil)

  ;; Add rebase argument to pull
  ;; https://github.com/magit/magit/issues/2597
  (magit-define-popup-switch 'magit-pull-popup ?R "Rebase" "--rebase")
  (defun +magit-submodule-remove (path &optional leave-in-work-tree)
    "Remove the submodule at PATH.
     https://stackoverflow.com/questions/1260748/how-do-i-remove-a-submodule"
    (interactive
     (list (magit-completing-read "Remove module" (magit-get-submodules)
                                  nil t nil nil (magit-section-when module))))
    (magit-with-toplevel
     ;; 0. mv a/submodule a/submodule_tmp
     (shell-command (format "mv %s %s_tmp" path path))

     ;; 1. git submodule deinit -f -- a/submodule
     (magit-run-git "submodule" "deinit" "-f" "--" path)

     ;; (magit-run-git-async "submodule" "deinit" path)

     ;; 2. rm -rf .git/modules/a/submodule
     (shell-command (format "rm -rf .git/modules/%s" path))

     (if (not leave-in-work-tree)
         ;; 3. git rm -f a/submodule
         (magit-run-git "rm" "-f" path)
       ;; # If you want to leave it in your working tree and have done step 0.
       ;; 3b. git rm --cached a/submodule
       ;; 3b. mv a/submodule_tmp a/submodule
       (magit-run-git "rm" "--cached" path)
       (shell-command-to-string (format "mv %s_tmp %s" path path)))))

  (magit-define-popup-action
   'magit-submodule-popup ?x "Remove" #'+magit-submodule-remove))

(use-package evil-ediff
  :commands (evil-ediff-init)
  :init
  (defun +evil-ediff-init ()
    "Initialize with `evil-ediff-init' and remove the hook."
    (evil-ediff-init)
    (remove-hook 'ediff-mode-hook #'evil-ediff-init))
  (add-hook 'ediff-mode-hook #'+evil-ediff-init)
  :config
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options "-w")
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(use-package evil-magit
  :init
  (setq evil-magit-want-horizontal-movement t))

(use-package ibuffer-sidebar
  :config
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face '(:family "InconsolateG for Powerline" :height 120)))

(use-package dired-subtree
  :commands (dired-subtree-toggle dired-subtree-cycle)
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package vscode-icon
  :straight (:host github
             :repo "jojojames/vscode-icon-emacs"
             :files (:defaults "icons" "source")))

(use-package dired-sidebar
  :after vscode-icon
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-width 30)
  (setq dired-sidebar-theme 'ascii)
  (setq dired-sidebar-face '(:family "InconsolateG for Powerline" :height 120)))

(use-package all-the-icons-dired
  :after dired-sidebar
  :commands (all-the-icons-dired-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package swift-mode)

(use-package swift3-mode)

(use-package kotlin-mode)

(use-package racket-mode)

(use-package flymake-racket
  :straight (:host github
             :repo "jojojames/flymake-racket")
  :commands (flymake-racket-add-hook)
  :init
  (add-hook 'scheme-mode-hook #'flymake-racket-add-hook)
  (add-hook 'racket-mode-hook #'flymake-racket-add-hook)
  (defun +scheme-mode-setup-linting ()
    (flymake-mode -1)
    (flycheck-mode -1))
  (add-hook 'scheme-mode-hook #'+scheme-mode-setup-linting)
  (add-hook 'racket-mode-hook #'+scheme-mode-setup-linting))

(use-package rainbow-mode)

(use-package vimrc-mode)

(use-package restclient)

;; end use-package configuration
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#252525" "#DADADA" "#C3C3C3" "#F6F6F6" "#E8E8E8" "#DADADA" "#F1F1F1" "#F6F6F6"])
 '(fci-rule-color "#171717")
 '(package-selected-packages
   (quote
    (spacegray-theme darkmine-theme oceanic-theme soft-morning-theme grandshell-theme zweilight-theme punpun-theme badger-theme omtose-phellack-theme busybee-theme phoenix-dark-pink-theme phoenix-dark-mono-theme inkpot-theme panda-theme ujelly-theme night-owl-theme gruvbox-theme underwater-theme madhat2r-theme darkburn-theme northcode-theme zerodark-theme nord nord-theme fold-dwim-org origami outshine esh-autosuggest go-mode ibuffer-vc ibuffer-projectile counsel-projectile counsel-tramp doom-modeline jazz-theme jbeans-theme klere-theme kooten-theme lenlen-theme mbo70s-theme melancholy-theme mellow-theme metalheart-theme mustang-theme solarized-theme sunburn-theme blackboard-theme bliss-theme bubbleberry-theme danneskjold-theme firecode-theme farmhouse-theme eziam-theme ibuffer-sidebar seoul257-theme twilight-bright-theme labburn-theme moe-theme borland-blue-theme autumn-light-theme switch-window restclient moom pkg one-themes ones-theme doneburn-theme plain-theme iodine-theme nofrils-acme-theme nofrils-acme groovy-mode gradle-mode rainbow-blocks rainbow-mode challenger-deep-theme kosmos-theme cosmos-theme habamax-theme kaolin-themes swift3-mode nimbus-theme hydandata-light-theme monotropic-theme darkokai-theme cyberpunk-theme objc-font-lock base16-themes base16 swift-mode darktooth-theme kotlin-mode csharp-mode doom hemisu-theme material-theme flatland-theme light-soap-theme yoshi-theme sexy-monochrome-theme paper-theme hc-zenburn-theme sourcerer-theme github-modern-theme green-is-the-new-black-theme greymatters-theme eclipse-theme distinguished-theme dark-mint-theme dakrone-light-theme cherry-blossom-theme atom-one-dark-theme atom-dark-theme ahungry-theme color-theme-approximate graphene-meta-theme spacemacs-theme elogcat which-key plan9-theme tao-theme eink-theme inverse-acme-theme gruber-darker-theme flatui-dark-theme flatui-theme leuven-theme creamsody-theme apropospriate-theme highlight-indent-guides evil-collection anti-zenburn zenburn markdown-mode sublimity-map sublimity diff-hl macrostep zenburn-theme anti-zenburn-theme minimap doom-themes dracula-theme projectile lispyville smartparens diminish evil-magit company multi-term magit all-the-icons-dired dired-sidebar dired-subtree tide web-mode exec-path-from-shell typescript-mode company-mode counsel ivy rainbow-delimiters hydra evil ht log4e dash)))
 '(vc-annotate-background "#0E0E0E")
 '(vc-annotate-color-map
   (quote
    ((20 . "#616161")
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
     (360 . "#FAFAFA"))))
 '(vc-annotate-very-old-color "#DADADA"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term-color-blue ((t (:background "#719899" :foreground "#719899"))))
 '(term-color-cyan ((t (:background "#009799" :foreground "#009799"))))
 '(term-color-green ((t (:background "#719872" :foreground "#719872"))))
 '(term-color-magenta ((t (:background "#$9A7599" :foreground "#9A7599"))))
 '(term-color-yellow ((t (:background "#727100" :foreground "#727100")))))
