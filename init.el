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
                    :family "Triplicate T4c"
                    :height duc/font-height
                    :weight 'normal
                    :width 'normal)

;; make mode-line taller
(defun +make-modeline-taller (&rest _)
  "Make the mode line taller."
  (dolist (sym '(mode-line mode-line-inactive))
    (set-face-attribute
     sym nil
     :family "Triplicate T3p"
     :height (- duc/font-height 20)
     :weight 'normal
     :width 'normal
     :underline (face-attribute sym :background)
     :box `(:line-width 3 :color ,(face-attribute `,sym :background)))))

(advice-add 'load-theme :after '+make-modeline-taller)

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

;; Personal Theming override for org-mode
;; Keep org-mode headers from being scaled by themes.
;; Plucked from [Disable enlarged Org mode header appearance]
;;             (https://emacs.stackexchange.com/questions/22584/)
(defun duc/org-mode-theme (&rest _)
  (ignore-errors
    (dolist (face '(org-level-1
                    org-level-2
                    org-level-3
                    org-level-4
                    org-level-5))
      (set-face-attribute face nil
                          :family "iA Writer Duospace"
                          :height 1.0))))

(advice-add 'load-theme :after 'duc/org-mode-theme)

;; package management
;;

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

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

;; Disable in favor of `use-package'.
(setq package-enable-at-startup nil)

(let ((default-directory "~/.emacs.d/")))

;; notes: counsel-fzf
(what-cursor-position)

;; Package declarations
;;

(use-package which-key
  :init
  (setq which-key-idle-delay 0.2)
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package evil
  :config
  (setq evil-want-C-u-scroll t)
  (define-key evil-normal-state-map (kbd "M-.") nil)
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
    ("o" switch-to-buffer "open/create buffer")
    ("r" revert-buffer "reload buffer")
    ("w" save-buffer "save buffer")
    ("k" kill-buffer "kill buffer"))
  (defhydra hydra-submenu-eval (:exit t)
    ("e" eval-last-sexp "eval sexp")
    ("p" eval-print-last-sexp "eval sexp & print")
    ("f" eval-defun "eval defun"))
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
    ("b" duc/sidebar-toggle "sidebar"))
  (defhydra hydra-submenu-help (:exit t :hint nil)
    "
^Describe^           ^Info^
^^^^^^^^-------------------------------------
_m_: mode            _p_: list packages
_k_: key             _a_: apropos
_f_: function
_v_: variable
_c_: face
"
    ("m" describe-mode)
    ("f" counsel-describe-function)
    ("v" counsel-describe-variable)
    ("k" describe-key)
    ("c" describe-face)
    ("p" package-list-packages)
    ("a" counsel-apropos))
  (defhydra hydra-submenu-customize-face (:exit t :hint nil)
    "
^Font^                  ^Face^                 ^Buffer^
^^^^^^^^----------------------------------------------------------------
_f_: font            _c_: describe face     _r_: hex colors
_s_: font size       _t_: theme             _w_: whitespace
_=_: font size +     ^ ^                    _l_: line-wrap
_-_: font size -
"
    ("f" duc/ivy-font)
    ("s" duc/font-size)
    ("=" duc/font-size-increase)
    ("-" duc/font-size-decrease)
    ("c" counsel-describe-face)
    ("t" counsel-load-theme)
    ("r" rainbow-mode)
    ("w" whitespace-mode)
    ("l" toggle-truncate-lines))
  (defhydra hydra-submenu-package (:exit t)
    ("r" package-refresh-contents "package-refresh-contents")
    ("i" straight-use-package "install")
    ("l" package-list-packages "package-list")
    ("d" package-delete "package-delete"))
  (defhydra hydra-submenu-git (:exit t :hint nil)
    "
              ^Git^
^^^^^^^^---------------------------------
_g_: status    _L_: log       _b_: blame
^ ^            _f_: file log
_j_: next      _u_: upper     _e_: ediff
_k_: prev      _l_: lower
"
    ("g" magit-status)
    ("L" magit-log)
    ("f" magit-log-buffer-file)
    ("b" magit-blame)
    ("j" smerge-next)
    ("k" smerge-prev)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("e" smerge-ediff))
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
_a_: jump      ^ ^                ^ ^
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
    ("n" switch-to-buffer)
    ("m" counsel-fzf)
    ("o" hydra-submenu-org-mode/body)
    ("p" counsel-projectile-switch-project)
    ("s" duc/ivy-eshell)
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

(use-package nofrils-acme-theme
  :disabled)

(use-package tao-theme
  :disabled
  :init
  (load-theme 'tao-yin t))

(use-package nord-theme
  :disabled
  :config
  (setq nord-region-highlight "snowstorm")
  (setq nord-comment-brightness 15))

(use-package zerodark-theme
  :disabled
  :init
  (load-theme 'zerodark t))

(use-package rainbow-delimiters
  :straight (:host github
             :repo "Fanael/rainbow-delimiters"))

(use-package seoul256-theme
  :init
  (setq seoul256-background 254))

(use-package habamax-theme
  :disabled)

(use-package undo-tree
  :diminish undo-tree-mode)

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

(use-package smartparens
  :disabled
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

  (use-package smartparens-config)
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
  (counsel-projectile-mode))

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
             counsel-fzf-occur)
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
  :after magit
  :init
  (setq evil-magit-want-horizontal-movement t))

(use-package ibuffer-sidebar
  :config
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face
        (cond
         ((eq system-type 'darwin)
          '(:family "Triplicate T3p" :height 130))
         ((eq system-type 'windows-nt)
          '(:family "Triplicate T3p" :height 130))
         (:default
          '(:family "Arial" :height 130)))))

(use-package dired-subtree
  :commands (dired-subtree-toggle dired-subtree-cycle)
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-width 30)
  (setq dired-sidebar-theme 'ascii)
  (setq dired-sidebar-face
        (cond
         ((eq system-type 'darwin)
          '(:family "Triplicate T3p" :height 130))
         ((eq system-type 'windows-nt)
          '(:family "Triplicate T3p" :height 130))
         (:default
          '(:family "Arial" :height 130)))))

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

(use-package rainbow-mode)

(use-package vimrc-mode)

;; end use-package configuration

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#E8E8E8" "#3C3C3C" "#616161" "#0E0E0E" "#252525" "#3C3C3C" "#171717" "#0E0E0E"])
 '(ansi-term-color-vector
   [unspecified "#fdf6e3" "#dc322f" "#859900" "#b58900" "#268bd2" "#6c71c4" "#268bd2" "#586e75"] t)
 '(beacon-color "#F8BBD0")
 '(company-quickhelp-color-background "#e8e8e8")
 '(company-quickhelp-color-foreground "#444444")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "ef1e992ef341e86397b39ee6b41c1368e1b33d45b0848feac6a8e8d5753daa67" "0e0c37ee89f0213ce31205e9ae8bce1f93c9bcd81b1bcda0233061bb02c357a8" "086970da368bb95e42fd4ddac3149e84ce5f165e90dfc6ce6baceae30cf581ef" "444238426b59b360fb74f46b521933f126778777c68c67841c31e0a68b0cc920" "31992d4488dba5b28ddb0c16914bf5726dc41588c2b1c1a2fd16516ea92c1d8e" "39dffaee0e575731c909bb3e4b411f1c4759c3d7510bf02aa5aef322a596dd57" "6be42070d23e832a7493166f90e9bb08af348a818ec18389c1f21d33542771af" "3481e594ae6866d72c40ad77d86a1ffa338d01daa9eb0977e324f365cef4f47c" "d61fc0e6409f0c2a22e97162d7d151dee9e192a90fa623f8d6a071dbf49229c6" "718fb4e505b6134cc0eafb7dad709be5ec1ba7a7e8102617d87d3109f56d9615" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "d494af9adbd2c04bec4b5c414983fefe665cd5dadc5e5c79fd658a17165e435a" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "0ee3fc6d2e0fc8715ff59aed2432510d98f7e76fe81d183a0eb96789f4d897ca" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "6fc0ae7cc2abd82d8add1140874ccf8773feaaae73a704981d52fdf357341038" "4b207752aa69c0b182c6c3b8e810bbf3afa429ff06f274c8ca52f8df7623eb60" "2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" "d2c61aa11872e2977a07969f92630a49e30975220a079cd39bec361b773b4eb3" "10e3d04d524c42b71496e6c2e770c8e18b153fcfcc838947094dad8e5aa02cef" default)))
 '(evil-emacs-state-cursor (quote ("#D50000" hbar)) t)
 '(evil-insert-state-cursor (quote ("#D50000" bar)) t)
 '(evil-normal-state-cursor (quote ("#F57F17" box)) t)
 '(evil-visual-state-cursor (quote ("#66BB6A" box)) t)
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#383838")
 '(fringe-mode 6 nil (fringe))
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   (quote
    ("#F57F17" "#66BB6A" "#0097A7" "#42A5F5" "#7E57C2" "#D84315")))
 '(highlight-symbol-foreground-color "#546E7A")
 '(highlight-tail-colors (quote (("#F8BBD0" . 0) ("#FAFAFA" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-background-colors (quote ("#e8fce8" "#c1e7f8" "#f8e8e8")))
 '(hl-paren-colors (quote ("#40883f" "#0287c8" "#b85c57")))
 '(jdee-db-active-breakpoint-face-colors (cons "#11171D" "#5EC4FF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#11171D" "#8BD49C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#11171D" "#41505E"))
 '(linum-format (quote dynamic))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#032f62" "#6a737d" "#d73a49" "#6a737d" "#005cc5" "#6f42c1" "#d73a49" "#6a737d")))
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (spacegray-theme darkmine-theme oceanic-theme soft-morning-theme grandshell-theme zweilight-theme punpun-theme badger-theme omtose-phellack-theme busybee-theme phoenix-dark-pink-theme phoenix-dark-mono-theme inkpot-theme panda-theme ujelly-theme night-owl-theme gruvbox-theme underwater-theme madhat2r-theme darkburn-theme northcode-theme zerodark-theme nord nord-theme fold-dwim-org origami outshine esh-autosuggest go-mode ibuffer-vc ibuffer-projectile counsel-projectile counsel-tramp doom-modeline jazz-theme jbeans-theme klere-theme kooten-theme lenlen-theme mbo70s-theme melancholy-theme mellow-theme metalheart-theme mustang-theme solarized-theme sunburn-theme blackboard-theme bliss-theme bubbleberry-theme danneskjold-theme firecode-theme farmhouse-theme eziam-theme ibuffer-sidebar seoul257-theme twilight-bright-theme labburn-theme moe-theme borland-blue-theme autumn-light-theme switch-window restclient moom pkg one-themes ones-theme doneburn-theme plain-theme iodine-theme nofrils-acme-theme nofrils-acme groovy-mode gradle-mode rainbow-blocks rainbow-mode challenger-deep-theme kosmos-theme cosmos-theme habamax-theme kaolin-themes swift3-mode nimbus-theme hydandata-light-theme monotropic-theme darkokai-theme cyberpunk-theme objc-font-lock base16-themes base16 swift-mode darktooth-theme kotlin-mode csharp-mode doom hemisu-theme material-theme flatland-theme light-soap-theme yoshi-theme sexy-monochrome-theme paper-theme hc-zenburn-theme sourcerer-theme github-modern-theme green-is-the-new-black-theme greymatters-theme eclipse-theme distinguished-theme dark-mint-theme dakrone-light-theme cherry-blossom-theme atom-one-dark-theme atom-dark-theme ahungry-theme color-theme-approximate graphene-meta-theme spacemacs-theme elogcat which-key plan9-theme tao-theme eink-theme inverse-acme-theme gruber-darker-theme flatui-dark-theme flatui-theme leuven-theme creamsody-theme apropospriate-theme highlight-indent-guides evil-collection anti-zenburn zenburn markdown-mode sublimity-map sublimity diff-hl macrostep zenburn-theme anti-zenburn-theme minimap doom-themes dracula-theme projectile lispyville smartparens diminish evil-magit company multi-term magit all-the-icons-dired dired-sidebar dired-subtree tide web-mode exec-path-from-shell typescript-mode company-mode counsel ivy rainbow-delimiters hydra evil ht log4e dash)))
 '(pdf-view-midnight-colors (quote ("#6a737d" . "#fffbdd")))
 '(pos-tip-background-color "#ffffffffffff")
 '(pos-tip-foreground-color "#78909C")
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945")
 '(rainbow-identifiers-choose-face-function (quote rainbow-identifiers-cie-l*a*b*-choose-face) t)
 '(rainbow-identifiers-cie-l*a*b*-color-count 1024 t)
 '(rainbow-identifiers-cie-l*a*b*-lightness 80 t)
 '(rainbow-identifiers-cie-l*a*b*-saturation 25 t)
 '(red "#ffffff")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sml/active-background-color "#98ece8")
 '(sml/active-foreground-color "#424242")
 '(sml/inactive-background-color "#4fa8a8")
 '(sml/inactive-foreground-color "#424242")
 '(tabbar-background-color "#ffffffffffff")
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background "#F6F6F6")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#C3C3C3")
     (40 . "#9E9E9E")
     (60 . "#9E9E9E")
     (80 . "#616161")
     (100 . "#616161")
     (120 . "#3C3C3C")
     (140 . "#3C3C3C")
     (160 . "#252525")
     (180 . "#252525")
     (200 . "#252525")
     (220 . "#171717")
     (240 . "#171717")
     (260 . "#171717")
     (280 . "#0E0E0E")
     (300 . "#0E0E0E")
     (320 . "#0E0E0E")
     (340 . "#090909")
     (360 . "#090909"))))
 '(vc-annotate-very-old-color "#3C3C3C")
 '(weechat-color-list
   (unspecified "#242728" "#424748" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff"))
 '(when
      (or
       (not
        (boundp
         (quote ansi-term-color-vector)))
       (not
        (facep
         (aref ansi-term-color-vector 0)))))
 '(window-divider-default-right-width 1)
 '(window-divider-mode t)
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
