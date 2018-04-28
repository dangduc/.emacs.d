;;; init.el --- duddang Initialization File
;;

;; duc namespace

(defmacro duc/alist-replace (list-var element)
  `(let
       ((replaced-list-var
         (assq-delete-all
          (car ',element) ,list-var)))
     (setq ,list-var
           (add-to-list 'replaced-list-var ',element))))

(defmacro duc/alist-replace-set (list-var element)
  `(setq ,list-var (duc/alist-replace ,list-var ,element)))

(defvar duc/font-height)
(setq duc/font-height 140)

;; font chooser
(defun duc/ivy-font ()
  (interactive)
  (set-face-attribute 'default nil
                      :family (completing-read "font: "
                                               (font-family-list))
                      :height duc/font-height
                      :weight 'normal
                      :width 'normal))

;; duc namespace (end)

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

;; enable transparent osx titlebar (a la Chrome)
(duc/alist-replace-set default-frame-alist (ns-transparent-titlebar . t))

;; nil or dark, to switch to between black or white title text
(duc/alist-replace-set default-frame-alist (ns-appearance . dark))

;(duc/alist-replace-set default-frame-alist (ns-use-thin-smoothing . t))
(duc/alist-replace-set default-frame-alist (ns-antialias-text . t))

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

;; make mode-line taller
(defun +make-modeline-taller (&rest _)
  "Make the mode line taller."
  (dolist (sym '(mode-line mode-line-inactive))
    (set-face-attribute
     sym nil
     :family "iA Writer Duospace"
     :height 120
     :weight 'normal
     :width 'normal
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

;; Disable in favor of `use-package'.
(setq package-enable-at-startup nil)

;; Separate package directories according to Emacs version.
;; Bytecode compiled in different Emacs versions are not
;; guaranteed to work with another.
(setq package-user-dir
      (format "%selpa/%s/" user-emacs-directory emacs-major-version))

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Setup built-in package manager
(setq package-archives '(("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Bootstrap use-package as package manager
(eval-when-compile
  (require 'use-package))

;; notes: counsel-fzf
(what-cursor-position)

;; Package declarations
;;

(use-package which-key
  :ensure t
  :init
  (setq which-key-idle-delay 0.2)
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package exec-path-from-shell
  ;; Set the shell environment properly.
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;(use-package highlight-indent-guides
;  :ensure t
;  :config
;  (setq highlight-indent-guides-method 'character)
;  :init
;  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;  (highlight-indent-guides-mode))

(use-package evil
  :ensure t
  :config
  (setq evil-want-C-u-scroll t)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  :init
  (setq evil-want-integration nil)
  (evil-mode 1))

(use-package evil-collection
    :ensure t
    :after evil
    :init
    (evil-collection-init))

(use-package general
  :ensure t
  :after evil
  :config
  (general-override-mode)
  (general-define-key
   :states '(normal motion visual)
   :keymaps 'override
   "<SPC>" 'hydra-main-menu/body))

(use-package hydra
  :ensure t
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
  (defhydra hydra-submenu-window (:exit t)
    ("fn" make-frame-command "new frame")
    ("ff" toggle-frame-fullscreen "toggle fullscreen")
    ("k" delete-window "kill window"))
  (defhydra hydra-submenu-file (:exit t)
    ("f" find-file "find file")
    ("w" save-buffer "write file")
    ("i" (find-file "~/.emacs.d/init.el" ) "init.el")
    ("b" dired-sidebar-toggle-sidebar "sidebar"))
  (defhydra hydra-submenu-help (:exit t)
    ("p" package-list-packages "list packages")
    ("a" counsel-apropos "apropos")
    ("k" describe-key "describe key")
    ("f" counsel-describe-function "describe function")
    ("v" counsel-describe-variable "describe variable"))
  (defhydra hydra-submenu-customize-face (:exit t)
    ("f" duc/ivy-font "change font")
    ("d" counsel-describe-face "describe face")
    ("t" counsel-load-theme "load theme"))
  (defhydra hydra-submenu-git (:exit t :hint nil)
    "
              ^Git^
^^^^^^^^---------------------------------
_g_: status    _l_: log      _b_: blame
"
    ("g" magit-status)
    ("l" magit-log)
    ("b" magit-blame))
  (defhydra hydra-main-menu (:exit t :idle .2 :hint nil)
    "
^Window^       ^Fuzzy^           ^Action^          ^Application
^^^^^^^^-----------------------------------------------------------------
_h_: left      _o_: contents   _SPC_: M-x          _g_: magit
_l_: right     _n_: buffers      _b_: buffers
_k_: up        _m_: files        _e_: eval
_j_: down      ^ ^               _w_: window
_\\_: vsplit   ^ ^                _?_: help
_-_: hsplit    ^ ^               _f_: file
^ ^            ^ ^               _c_: customize
"
    ("h" evil-window-left)
    ("l" evil-window-right)
    ("k" evil-window-up)
    ("j" evil-window-down)
    ("-" split-window-below)
    ("\\" split-window-right)
    ("o" counsel-rg)
    ("n" switch-to-buffer)
    ("m" counsel-fzf)
    ("SPC" execute-extended-command "M-x")
    ("b" hydra-submenu-buffer/body)
    ("c" hydra-submenu-customize-face/body)
    ("e" hydra-submenu-eval/body)
    ("w" hydra-submenu-window/body)
    ("?" hydra-submenu-help/body)
    ("f" hydra-submenu-file/body)
    ("g" hydra-submenu-git/body)))

(use-package plan9-theme
  :disabled
  :ensure t
  :init
  (load-theme 'plan9 t)

  (with-eval-after-load 'whitespace-mode
    (set-face-attribute 'whitespace-line nil
                        :foreground "black"
                        :background "#e6e6d1"))
  (when (boundp 'window-divider-mode)
    (setq window-divider-default-places t
          window-divider-default-bottom-width 1
          window-divider-default-right-width 1)
    (window-divider-mode +1))
  (set-face-attribute 'mode-line-buffer-id nil
                      :foreground "#424242")
  (set-face-attribute 'mode-line nil
                      :background "#e5fbff"
                      :box '(:line-width 3 :color "#e5fbff"))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#888888"
                      :background "#e5fbff"
                      :box '(:line-width 3 :color "#e5fbff"))
  (set-face-attribute 'vertical-border nil
                      :foreground "#000000")
  (set-face-attribute 'fringe nil
                      ;:background "#efeccb"
                      :background "#FFFFE8"))

(use-package seoul256-theme
  :disabled
  :ensure t
  :init
  (setq seoul256-background 235)
  (set-face-attribute  'vertical-border nil
                       :foreground "#323232")
  (load-theme 'seoul256 t))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

(use-package whitespace
  :ensure t
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
  :ensure t
  :init
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "C-;") 'macrostep-collapse)
    (define-key evil-normal-state-map (kbd "C-'") 'macrostep-expand)))

(use-package diminish
  :ensure t
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
  :ensure t
  :diminish smartparens-mode
  :init
  (dolist (hook '(lisp-mode-hook
                  scheme-mode-hook
                  clojure-mode-hook
                  emacs-lisp-mode-hook))
    (add-hook hook #'smartparens-strict-mode))
  :config
  ;; Disable highlights.
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (setq sp-cancel-autoskip-on-backward-movement nil
        sp-autoskip-closing-pair 'always-end
        sp-autoskip-opening-pair t)

  (use-package smartparens-config :ensure nil)
  (smartparens-global-mode 1)
  (sp-pair "(" ")" :wrap "M-(")
  (sp-pair "(" ")" :wrap "M-)")
  (sp-pair "[" "]" :wrap "M-[")
  (sp-pair "[" "]" :wrap "M-]")
  (sp-pair "{" "}" :wrap "M-{")
  (sp-pair "{" "}" :wrap "M-}")
  (sp-pair "\"" "\"" :wrap "M-\""))

(use-package lispyville
  :ensure t
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
  :ensure t
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

(use-package rainbow-delimiters
  :ensure t
  :config (setq show-paren-delay 0)
  (show-paren-mode 1)
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; Terminal
(use-package multi-term
  :ensure t
  :commands (multi-term multi-term-next multi-term-prev)
  :config
  (evil-define-key 'insert term-raw-map (kbd "TAB") 'term-send-raw) ;; rebinding
  (evil-define-key 'normal term-raw-map (kbd "p") 'term-paste)
  (define-key term-raw-map (kbd "C-y") 'term-paste)
  (add-to-list 'term-unbind-key-list "C-q") ; C-q binds to raw input by default
  (setq multi-term-program "/bin/zsh"))

(use-package ivy
  :ensure t
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

  (ivy-mode))

(use-package counsel
  :ensure t
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
  :ensure t
  :commands (swiper)
  :diminish ivy-mode)

(use-package flycheck
  :if (not (eq system-type 'windows-nt))
  :defer 8
  :ensure t
  :diminish flycheck-mode
  :commands (flycheck-mode)
  :config
  (setq flycheck-idle-change-delay 1.2)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (global-flycheck-mode))

(use-package company
  :ensure t
  :diminish company-mode
  :config (company-tng-configure-default)
  (setq company-idle-delay .01)
  (setq company-minimum-prefix-length 1)
  (global-company-mode))

(use-package web-mode
  :ensure t
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
  :ensure t
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
  :ensure t
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
  :ensure t
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
  :ensure t
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
  :ensure t
  :after magit
  :init
  (setq evil-magit-want-horizontal-movement t))

(use-package dired-subtree
  :ensure t
  :commands (dired-subtree-toggle dired-subtree-cycle)
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-width 30)
  (setq dired-sidebar-theme 'ascii)
  (setq dired-sidebar-face
        (cond
         ((eq system-type 'darwin)
          '(:family "iA Writer Duospace" :height 130))
         ((eq system-type 'windows-nt)
          '(:family "iA Writer Duospace" :height 130))
         (:default
          '(:family "Arial" :height 130)))))

(use-package all-the-icons-dired
  :ensure t
  :after dired-sidebar
  :commands (all-the-icons-dired-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package swift-mode
  :ensure t)

(use-package swift3-mode
  :ensure t)

;; end use-package configuration

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#E8E8E8" "#3C3C3C" "#616161" "#0E0E0E" "#252525" "#3C3C3C" "#171717" "#0E0E0E"])
 '(ansi-term-color-vector
   [unspecified "#fdf6e3" "#dc322f" "#859900" "#b58900" "#268bd2" "#6c71c4" "#268bd2" "#586e75"] t)
 '(beacon-color "#F8BBD0")
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("ef1e992ef341e86397b39ee6b41c1368e1b33d45b0848feac6a8e8d5753daa67" "0e0c37ee89f0213ce31205e9ae8bce1f93c9bcd81b1bcda0233061bb02c357a8" "086970da368bb95e42fd4ddac3149e84ce5f165e90dfc6ce6baceae30cf581ef" "444238426b59b360fb74f46b521933f126778777c68c67841c31e0a68b0cc920" "31992d4488dba5b28ddb0c16914bf5726dc41588c2b1c1a2fd16516ea92c1d8e" "39dffaee0e575731c909bb3e4b411f1c4759c3d7510bf02aa5aef322a596dd57" "6be42070d23e832a7493166f90e9bb08af348a818ec18389c1f21d33542771af" "3481e594ae6866d72c40ad77d86a1ffa338d01daa9eb0977e324f365cef4f47c" "d61fc0e6409f0c2a22e97162d7d151dee9e192a90fa623f8d6a071dbf49229c6" "718fb4e505b6134cc0eafb7dad709be5ec1ba7a7e8102617d87d3109f56d9615" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "d494af9adbd2c04bec4b5c414983fefe665cd5dadc5e5c79fd658a17165e435a" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "0ee3fc6d2e0fc8715ff59aed2432510d98f7e76fe81d183a0eb96789f4d897ca" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "6fc0ae7cc2abd82d8add1140874ccf8773feaaae73a704981d52fdf357341038" "4b207752aa69c0b182c6c3b8e810bbf3afa429ff06f274c8ca52f8df7623eb60" "2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" "d2c61aa11872e2977a07969f92630a49e30975220a079cd39bec361b773b4eb3" "10e3d04d524c42b71496e6c2e770c8e18b153fcfcc838947094dad8e5aa02cef" default)))
 '(evil-emacs-state-cursor (quote ("#D50000" hbar)) t)
 '(evil-insert-state-cursor (quote ("#D50000" bar)) t)
 '(evil-normal-state-cursor (quote ("#F57F17" box)) t)
 '(evil-visual-state-cursor (quote ("#66BB6A" box)) t)
 '(fci-rule-color "#383838")
 '(fringe-mode 6 nil (fringe))
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   (quote
    ("#F57F17" "#66BB6A" "#0097A7" "#42A5F5" "#7E57C2" "#D84315")))
 '(highlight-symbol-foreground-color "#546E7A")
 '(highlight-tail-colors (quote (("#F8BBD0" . 0) ("#FAFAFA" . 100))))
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
    (kaolin-themes swift3-mode nimbus-theme hydandata-light-theme monotropic-theme darkokai-theme cyberpunk-theme objc-font-lock base16-themes base16 base16-theme swift-mode darktooth-theme kotlin-mode csharp-mode doom hemisu-theme material-theme flatland-theme light-soap-theme yoshi-theme sexy-monochrome-theme paper-theme hc-zenburn-theme sourcerer-theme github-modern-theme green-is-the-new-black-theme greymatters-theme eclipse-theme distinguished-theme dark-mint-theme dakrone-light-theme cherry-blossom-theme atom-one-dark-theme atom-dark-theme ahungry-theme color-theme-approximate graphene-meta-theme spacemacs-theme elogcat which-key plan9-theme tao-theme eink-theme inverse-acme-theme sublime-themes gruber-darker-theme flatui-dark-theme flatui-theme leuven-theme creamsody-theme apropospriate-theme highlight-indent-guides evil-collection anti-zenburn zenburn markdown-mode sublimity-map sublimity diff-hl macrostep zenburn-theme anti-zenburn-theme minimap doom-themes dracula-theme projectile lispyville smartparens diminish evil-magit company multi-term magit all-the-icons-dired dired-sidebar dired-subtree tide web-mode exec-path-from-shell typescript-mode company-mode counsel ivy rainbow-delimiters hydra evil seoul256-theme ht log4e dash)))
 '(pdf-view-midnight-colors (quote ("#6a737d" . "#fffbdd")))
 '(pos-tip-background-color "#ffffffffffff")
 '(pos-tip-foreground-color "#78909C")
 '(red "#ffffff")
 '(sml/active-background-color "#98ece8")
 '(sml/active-foreground-color "#424242")
 '(sml/inactive-background-color "#4fa8a8")
 '(sml/inactive-foreground-color "#424242")
 '(tabbar-background-color "#ffffffffffff")
 '(vc-annotate-background "#F6F6F6")
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
 '(window-divider-default-right-width 1)
 '(window-divider-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
