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

