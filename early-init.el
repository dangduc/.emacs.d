;;;; -*- lexical-binding: t; -*-

(require 'package)

;; Defined in Emacs 27 and above.
(defvar package-quickstart)

;; Defer garbage collection further back in the startup process.
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq initial-frame-alist
      (cond
       ((eq system-type 'gnu/linux)
        '((width . 90) (height . 45)))
       ((eq system-type 'windows-nt)
        '((width . 92) (height . 46)))
       (:else
        '((width . 100) (height . 44)))))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Separate package directories according to Emacs version.
;; Bytecode compiled in different Emacs versions are not
;; guaranteed to work with another.
(setq package-user-dir
      (format "%selpa/%s/" user-emacs-directory emacs-major-version))

(setq load-prefer-newer t)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "vendor/packed" dir))
  (add-to-list 'load-path (expand-file-name "vendor/auto-compile" dir)))
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(setq package-enable-at-startup nil)

;; Package Repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

(setq package-archive-priorities '(("org" . 15)
                                   ("melpa" . 10)
                                   ("melpa-stable" . 5)
                                   ("gnu" . 1)))

;; Activate all packages (in particular autoloads).
;; Use `package-quickstart' feature in Emacs 27 so we only need to
;; `package-initialize' if on Emacs 26 and below.
;; Take a look at $EMACS_CODEBASE/lisp/startup.el to refresh your memory.
;; The gist is that `package-activate-all' is called in Emacs 27 which
;; reads `package-quickstart'.
(if (>= emacs-major-version 27)
    (setq package-quickstart t)
  (package-initialize))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;; We provide `early-init' so that Emacs 26 and below can reuse this file.
;; In Emacs 27, this file is loaded automatically.
(provide 'early-init)
