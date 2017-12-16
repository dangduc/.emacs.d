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

;; duc namespace (end)

;; (package-initialize)

(setq gc-cons-threshold 100000000) ; 100 mb

;; Get rid of extraneous UI
;;(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(duc/alist-replace-set default-frame-alist (ns-transparent-titlebar . t))

;; nil or dark, to switch to between black or white title text
(duc/alist-replace-set default-frame-alist (ns-appearance . dark))

;;(add-to-list 'default-frame-alist '(ns-use-thin-smoothing . t))
(duc/alist-replace-set default-frame-alist (ns-use-thin-smoothing . t))

;; line numbers (emacs 26 and above)
(global-display-line-numbers-mode)

;; Set font
;; To see current font M-x (face-attribute 'default :font)
(set-face-attribute 'default nil
                    ;; :family "Input Mono"
                    :family "Input Mono"
                    :height 180
                    :weight 'normal
                    :width 'normal)

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

;; Package declarations
;;

(use-package exec-path-from-shell
  ;; Set the shell environment properly.
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

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

(use-package seoul256-theme
  :ensure t
  :init
  (setq seoul256-background 235)
  (with-eval-after-load 'ivy
    (set-face-attribute 'mode-line nil
                        :weight 'bold
                        :height .97
                        :box `(
                               :line-width 3
                               :color "#a1706f"))
    (set-face-attribute 'mode-line-inactive nil
                        :height .96
                        :box `(
                               :line-width 3
                               :color "#a1706f")))
  (load-theme 'seoul256 t))

(use-package macrostep
  :ensure t
  :init
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "C-;") 'macrostep-collapse)
    (define-key evil-normal-state-map (kbd "C-'") 'macrostep-expand)
    ))

(use-package evil
  :ensure t
  :config (evil-mode 1)
  (setq evil-want-C-u-scroll t)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  ;(define-key evil-normal-state-map (kbd "C-o") 'counsel-fzf)
  ;(define-key evil-normal-state-map (kbd "C-n") 'counsel-fzf)
  (define-key evil-normal-state-map (kbd "C-m") 'counsel-fzf)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-\\") 'evil-window-vsplit)
  (define-key evil-normal-state-map (kbd "C--") 'evil-window-split)
  ;(define-key evil-normal-state-map (kbd "<SPC>") 'hydra-main-menu/body)
  (use-package general
    :ensure t
    :config
    (general-override-mode)
    (general-define-key
     :states '(normal motion visual)
     :keymaps 'override
     "<SPC>" 'hydra-main-menu/body)))

(use-package hydra
  :ensure t
  :config (defhydra hydra-submenu-buffer (:exit t)
            ("p" previous-buffer "prev buffer")
            ("n" next-buffer "next buffer")
            ("l" list-buffers "list buffers")
            ("o" switch-to-buffer "open/create buffer")
            ("s" save-buffer "save buffer")
            ("k" kill-buffer "kill buffer"))
  (defhydra hydra-submenu-eval (:exit t)
    ("e" eval-last-sexp "eval sexp")
    ("p" eval-print-last-sexp "eval sexp & print")
    ("f" eval-defun "eval defun"))
  (defhydra hydra-submenu-window (:exit t)
    ("d" delete-window "delete window"))
  (defhydra hydra-submenu-help (:exit t)
    ("p" package-list-packages)
    ("q" package)
    ("a" apropos)
    ("c" describe-command)
    ("f" describe-function)
    ("v" describe-variable))
  (defhydra hydra-main-menu (:exit t)
    ("SPC" execute-extended-command "M-x")
    ("b" hydra-submenu-buffer/body "buffer")
    ("e" hydra-submenu-eval/body "eval")
    ("w" hydra-submenu-window/body "window")
    ("h" hydra-submenu-help/body "help")))

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
     (additional-movement normal)
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
             counsel-fzf)
  :init
  (setq projectile-switch-project-action 'counsel-fzf)
  :config
  (ivy-set-prompt 'counsel-fzf (lambda () "> "))
  (setenv "FZF_DEFAULT_COMMAND"
          "(git ls-files --exclude-standard --others --cached ||
        find . -maxdepth 2 -path \"*/\\.*\" -prune -o -print -o -type l -print |
           sed s/^..//) 2> /dev/null")
  (setq counsel-async-filter-update-time 100000)
  (setq counsel-git-cmd "git ls-files --exclude-standard --full-name --others --cached --")
  (setq counsel-rg-base-command "rg --max-columns 80 -i --no-heading --line-number --color never %s .")
  (setq counsel-ag-base-command "ag -U --nocolor --nogroup %s -- ."))

(use-package swiper
  :ensure t
  :commands (swiper)
  ;;  :bind (:map evil-normal-state-map
  ;;              ("C-s" . swiper))
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
  :diminish company
  :config (company-tng-configure-default)
  (setq company-idle-delay .01)
  (setq company-minimum-prefix-length 1)
  (global-company-mode))

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

  ;; Save buffers automatically instead of asking.
  (setq magit-save-repository-buffers 'dontask)

  (setq magit-repository-directories '("~/dev" "~/.emacs.d"))
  (setq magit-refresh-status-buffer nil)

  ;; Add rebase argument to pull
  ;; https://github.com/magit/magit/issues/2597
  (magit-define-popup-switch 'magit-pull-popup ?R "Rebase" "--rebase"))

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
  (when (eq system-type 'windows-nt)
    (setq dired-sidebar-use-all-the-icons nil))

  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-face
        (cond
         ((eq system-type 'darwin)
          '(:family "Helvetica" :height 160))
         ((eq system-type 'windows-nt)
          '(:family "Times New Roman" :height 130))
         (:default
          '(:family "Arial" :height 140))))

  (use-package all-the-icons-dired
    ;; M-x all-the-icons-install-fonts
    :ensure t
    :commands (all-the-icons-dired-mode)))

;; end use-package configuration

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6fc0ae7cc2abd82d8add1140874ccf8773feaaae73a704981d52fdf357341038" "4b207752aa69c0b182c6c3b8e810bbf3afa429ff06f274c8ca52f8df7623eb60" "2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" "d2c61aa11872e2977a07969f92630a49e30975220a079cd39bec361b773b4eb3" "10e3d04d524c42b71496e6c2e770c8e18b153fcfcc838947094dad8e5aa02cef" default)))
 '(package-selected-packages
   (quote
    (macrostep zenburn-theme anti-zenburn-theme minimap doom-themes dracula-theme projectile lispyville smartparens diminish evil-magit company multi-term magit all-the-icons-dired dired-sidebar dired-subtree tide web-mode exec-path-from-shell typescript-mode company-mode counsel ivy rainbow-delimiters hydra evil seoul256-theme ht log4e dash))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
