;;; init.el --- duddang Initialization File
;;

;; (package-initialize)

(setq gc-cons-threshold 100000000) ; 100 mb

;; Get rid of extraneous UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t
  inhibit-startup-echo-area-message t) 
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; or 'dark, to switch to white title text
(add-to-list 'default-frame-alist '(ns-appearance . 'nil)) 
(add-to-list 'default-frame-alist '(ns-use-thin-smoothing . t))

;; Set font
;; To see current font M-x (face-attribute 'default :font)
(set-face-attribute 'default nil
                    :family "Input Mono"
                    :height 140
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
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap use-package as package manager
(require 'use-package)
(require 'req-package)

;; Package declarations
;;

(req-package seoul256-theme
  :ensure t
  :init (setq seoul256-background 255)
        (load-theme 'seoul256 t))
(req-package evil
  :require hydra
  :ensure t
  :config (evil-mode 1)
          (define-key evil-normal-state-map (kbd "<SPC>") 'hydra-main-menu/body))
(req-package hydra
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
            ("a" apropos)
            ("c" describe-command)
            ("f" describe-function)
            ("v" describe-variable))
          (defhydra hydra-main-menu (:exit t)
            ("SPC" execute-extended-command "M-x")
            ("b" hydra-submenu-buffer/body "buffer")
            ("e" hydra-submenu-eval/body "eval")
            ("w" hydra-submenu-window/body "window")
            ("h" hydra-submenu-help "help")))
(req-package rainbow-delimiters
  :ensure t
  :config (setq show-paren-delay 0)
          (show-paren-mode 1)
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(req-package-finish)

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers nil)
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
  :bind (:map evil-normal-state-map
              ("C-s" . swiper))
  :diminish ivy-mode) 

(use-package company-mode
  :ensure t
  :init (global-company-mode))

(use-package typescript-mode
  ;; npm install -g typescript
  :ensure t
  :mode
  ("\\.ts\\'" . typescript-mode)
  ("\\.ts$\\'" . typescript-mode)
  :init
  (add-hook 'typescript-mode-hook
            (lambda ()
              (setq-local typescript-indent-level (+indent-offset))
              (with-eval-after-load 'evil
                (setq-local evil-shift-width typescript-indent-level))))
  :config
  (setq typescript-enabled-frameworks '(typescript)))

(use-package typescript-mode
  ;; npm install -g typescript
  :ensure t
  :mode
  ("\\.ts\\'" . typescript-mode)
  ("\\.ts$\\'" . typescript-mode)
  :init
  (add-hook 'typescript-mode-hook
            (lambda ()
              (setq-local typescript-indent-level (+indent-offset))
              (with-eval-after-load 'evil
                (setq-local evil-shift-width typescript-indent-level))))
  :config
  (setq typescript-enabled-frameworks '(typescript)))

;; end use-package configuration

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (typescript-mode company-mode counsel ivy rainbow-delimiters hydra evil seoul256-theme ht log4e dash))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
