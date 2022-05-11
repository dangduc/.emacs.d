;;;; -*- lexical-binding: t; -*-

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

;; enable transparent osx titlebar (a la Chrome)
(push '(ns-transparent-titlebar . t) default-frame-alist)

;; nil or dark, to switch to between black or white title text
;; e.g. (ns-appearance . dark|nil)
(push '(ns-appearance . nil) default-frame-alist)

(push '(ns-use-thin-smoothing . t) default-frame-alist)
(push '(ns-antialias-text . nil) default-frame-alist)

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

(global-hl-line-mode -1)
(setq global-hl-line-sticky-flag nil)

;; helps Emacs keep up with fast input. This is another attempt to
;; solve the problem of handling high key repeat rate and other "slow
;; scrolling" situations.
(if (>= emacs-major-version 28)
    (setq redisplay-skip-fontification-on-input t))

;; fix some systems not properly highlighting text selection.
(if (string-equal (face-attribute 'default :foreground) "#000000")
    (set-face-attribute 'region nil :background "#dedede"))

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


(when (< emacs-major-version 28)
  (pixel-scroll-precision-mode 1))
;; Start emacs server
;; e.g. ~/.zshrc
;;   # (find-file) send to emacs server.
;;   # ie, emacsclient -n file1 file2 ...
;;   alias emacsff="emacsclient -n"
(if (not (eq system-type 'windows-nt))
    (and window-system (server-start)))

;; Don't open buffer for native compilation warnings.
(setq warning-suppress-types '((comp)))

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

(load-file (expand-file-name "local-declarations.el" user-emacs-directory))
(let ((local-properties (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-properties) (load-file local-properties)))

;; Vendor management
;;

;; Stolen from [Aaron Bedra's Emacs 26 Configuration](http://aaronbedra.com/emacs.d/#vendor-directory)
;; Setup up vendor directory.
(defvar duc/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path duc/vendor-dir)

(dolist (project (directory-files duc/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

; n/a

;; package management
;;

(when (< emacs-major-version 27)
  (setq package-enable-at-startup nil)
  ;; (package-initialize)
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

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

(load-file (expand-file-name "package-declarations.el" user-emacs-directory))

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
 '(custom-safe-themes
   '("e27c391095dcee30face81de5c8354afb2fbe69143e1129109a16d17871fc055" default))
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
 '(lin-mode-hooks '(tide-mode-hook elisp-mode))
 '(lsp-ui-doc-border "#93a1a1")
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(org-agenda-files '("~/dev/notes/log.org"))
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (R . t)
     (ditaa . t)
     (plantuml . t)))
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
