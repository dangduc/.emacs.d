;;;; -*- lexical-binding: t; -*-

(when (eq system-type 'darwin)
  ;; libgccjit needs Homebrew's GCC driver on PATH to locate its runtime
  ;; libraries.  GUI startup can compile Lisp before the asynchronous shell
  ;; environment import finishes, so supply these paths before any require.
  (dolist (directory '("/usr/local/bin" "/opt/homebrew/bin"))
    (when (file-directory-p directory)
      (add-to-list 'exec-path directory)
      (unless (member directory (split-string (or (getenv "PATH") "") path-separator t))
        (setenv "PATH" (concat directory path-separator (getenv "PATH")))))))

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

;; Version directories accept shared configuration links and independent sources.
(defvar user-lisp-directory nil)
(setq package-user-dir
      (expand-file-name (format "elpa/%s/" emacs-major-version)
                        user-emacs-directory)
      user-lisp-directory
      (file-name-as-directory
       (expand-file-name
        (if (memq system-type '(cygwin windows-nt ms-dos))
            "user-lisp"
          (format "user-lisp-%s" emacs-major-version))
        user-emacs-directory)))

;; Package activation happens in init.el.  Compile user Lisp after that,
;; when use-package and the macros in the configuration are available.
(setq user-lisp-auto-scrape nil)
(load (expand-file-name "user-lisp/duc-startup.el" user-emacs-directory)
      nil t t)
(duc-startup-initialize)
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name (format "eln-cache/%s/" emacs-major-version)
                     user-emacs-directory)))

;; Separate server sockets let agents address a specific Emacs version.
;; Emacs 31 keeps the default socket for existing emacsclient commands.
;; Preserve explicit --daemon=NAME / --fg-daemon=NAME selections.
(require 'server)
(unless (stringp (daemonp))
  (setq server-name (if (= emacs-major-version 31)
                        "server"
                      (format "emacs-%s" emacs-major-version))))

;; `load-prefer-newer' makes Emacs pick FOO.el over an older FOO.elc.  In an
;; installed macOS bundle the shipped Lisp is gzipped, so when jka-compr.el.gz
;; is not older than jka-compr.elc that rule sends Emacs to load jka-compr from
;; a .gz -- which needs jka-compr -- and startup dies with "Recursive load".
;; Pull it in here, while the nil default still applies and the .elc wins.
;; (`cp -R' of the .app collapses both mtimes to the copy time and triggers
;; exactly this; `cp -Rp' preserves the build's ordering.)
(require 'jka-compr)

(setq load-prefer-newer t)

;; Compilation uses the versioned links after init.  Global auto-compile
;; would also write bytecode beside shared source files opened for editing.

;; Compiler workers start without the parent session's loaded definitions.
;; Optional functions and macros can therefore produce unresolved-reference
;; warnings.  Keep these warnings in the logs without opening *Warnings*.
;; Actual compiler errors still require investigation.
(setq native-comp-async-report-warnings-errors 'silent)

(setq package-enable-at-startup nil)

;; Package Repositories
;; NOTE: elpa.gnu.org / elpa.nongnu.org (both 209.51.188.89) were unreachable
;; from some networks for a while and the GNU/NonGNU archives were served via
;; the Tsinghua (Tuna) mirror instead. Direct connectivity is back, and the
;; mirror now answers 403 for package tarballs (its archive-contents still
;; works, so installs fail only at download time), so these point at the
;; canonical archives again.
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("org" . "https://orgmode.org/elpa/")))

(setq package-archive-priorities '(("org" . 15)
                                   ("melpa" . 10)
                                   ("melpa-stable" . 5)
                                   ("gnu" . 1)
                                   ("nongnu" . 1)))

;; Emacs 30 ships many packages built-in (transient, compat, etc.). By default
;; package.el refuses to upgrade built-ins from ELPA, which breaks Magit's
;; `transient >= 0.13' requirement. Allow built-in packages to be upgraded.
(setq package-install-upgrade-built-in t)

;; Activate all packages (in particular autoloads).
;; Use `package-quickstart' feature in Emacs 27 so we only need to
;; `package-initialize' if on Emacs 26 and below.
;; Take a look at $EMACS_CODEBASE/lisp/startup.el to refresh your memory.
;; The gist is that `package-activate-all' is called in Emacs 27 which
;; reads `package-quickstart'.
;; The quickstart file needs the same per-version split as `package-user-dir'
;; above: it hard-codes paths inside `package-user-dir' and is byte-compiled by
;; whichever Emacs wrote it, so a single shared file makes each version clobber
;; the other's (Emacs 31 writing elpa/31 paths breaks an Emacs 30 startup).
(if (>= emacs-major-version 27)
    (setq package-quickstart t
          package-quickstart-file
          (format "%spackage-quickstart-%s.el"
                  user-emacs-directory emacs-major-version))
  (package-initialize))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;; We provide `early-init' so that Emacs 26 and below can reuse this file.
;; In Emacs 27, this file is loaded automatically.
(provide 'early-init)
