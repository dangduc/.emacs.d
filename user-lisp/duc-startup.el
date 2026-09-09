;;; duc-startup.el --- Version-specific Lisp sources and compilation -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar user-lisp-directory nil)
(defvar package-user-dir)
(defvar duc-user-lisp-source-directory
  (expand-file-name "user-lisp/" user-emacs-directory))
(defvar duc-vendor-directory
  (expand-file-name (format "vendor-%s/" emacs-major-version)
                    user-emacs-directory)
  "Persistent vendor sources for this Emacs major version.")

(defun duc-startup--link (source target)
  "Create or normalize the shared SOURCE link at TARGET.
Existing regular files and explicit links are version-specific overrides.
Return non-nil when the link changes."
  (make-directory (file-name-directory target) t)
  (let* ((link (file-symlink-p target))
         (relative (file-relative-name source (file-name-directory target))))
    (cond
     ((and link
           (equal (expand-file-name link (file-name-directory target)) source)
           (not (equal link relative)))
      (make-symbolic-link relative target t)
      t)
     ((not (or (file-exists-p target) link))
      (make-symbolic-link relative target)
      t))))

(defun duc-startup--shared-link-p (file)
  "Return non-nil if FILE is a managed link to its shared source."
  (when-let* ((target (file-symlink-p file)))
    (equal (expand-file-name target (file-name-directory file))
           (expand-file-name (file-relative-name file user-lisp-directory)
                             duc-user-lisp-source-directory))))

(defun duc-startup-sync-user-lisp ()
  "Update shared source links while preserving version-specific overrides."
  (unless (equal (file-truename duc-user-lisp-source-directory)
                 (file-truename user-lisp-directory))
    (let (changed)
      (dolist (source (directory-files-recursively
                       duc-user-lisp-source-directory "\\.el\\'"))
        (unless (string-prefix-p "." (file-name-nondirectory source))
          (when (duc-startup--link
                 source (expand-file-name
                         (file-relative-name source duc-user-lisp-source-directory)
                         user-lisp-directory))
            (setq changed t))))
      (dolist (link (directory-files-recursively user-lisp-directory "\\.el\\'"))
        (when (and (duc-startup--shared-link-p link) (not (file-exists-p link)))
          (setq changed t)
          (delete-file link)
          (when (file-exists-p (concat link "c"))
            (delete-file (concat link "c")))))
      ;; Do not activate autoloads for removed files before init.el runs.
      (when changed
        (dolist (name '(".user-lisp-autoloads.el" ".user-lisp-autoloads.elc"))
          (let ((file (expand-file-name name user-lisp-directory)))
            (when (file-exists-p file) (delete-file file))))))))

(defun duc-startup-initialize ()
  "Create this Emacs version's directories before package activation."
  (let ((fresh-packages (not (file-directory-p package-user-dir))))
    (make-directory package-user-dir t)
    (make-directory user-lisp-directory t)
    (when fresh-packages
      ;; Cached use-package expansions can omit installation side effects.
      (dolist (file (directory-files-recursively user-lisp-directory "\\.elc\\'"))
        (delete-file file))
      (dolist (suffix '(".el" ".elc"))
        (let ((file (expand-file-name
                     (format "package-quickstart-%s%s" emacs-major-version suffix)
                     user-emacs-directory)))
          (when (file-exists-p file) (delete-file file)))))
    (duc-startup-sync-user-lisp)
    (duc-startup-activate-user-lisp)))

(defun duc-startup--package-paths (root)
  "Return Lisp load directories under ROOT without traversing package assets.
Support loose files, package directories, and conventional lisp subdirectories."
  (when (file-directory-p root)
    (let ((paths (list (directory-file-name root))))
      (dolist (dir (directory-files root t "\\`[^.]"))
        (when (file-directory-p dir)
          (when (directory-files dir nil "\\.el\\'")
            (push dir paths))
          (let ((lisp (expand-file-name "lisp" dir)))
            (when (file-directory-p lisp) (push lisp paths)))))
      (nreverse paths))))

(defun duc-startup-activate-user-lisp ()
  "Give this version's local files priority over archive and vendor packages."
  (dolist (dir (duc-startup--package-paths
                (expand-file-name "extra" user-lisp-directory)))
    (add-to-list 'load-path dir))
  (add-to-list 'load-path (directory-file-name user-lisp-directory)))

(defun duc-startup-activate-vendor ()
  "Activate this major version's independent vendor sources.
Startup never copies or updates these sources from the development checkouts."
  (unless (file-directory-p duc-vendor-directory)
    (error "Missing %s; create this version's vendor sources before startup"
           duc-vendor-directory))
  (dolist (dir (duc-startup--package-paths duc-vendor-directory))
    (add-to-list 'load-path dir))
  (duc-startup-activate-user-lisp))

(defun duc-startup-local-library-p (name)
  "Return non-nil if library NAME comes from this version's local sources."
  (when-let* ((file (locate-library (symbol-name name))))
    (or (string-prefix-p (file-name-as-directory user-lisp-directory) file)
        (string-prefix-p (file-name-as-directory duc-vendor-directory) file))))

(defun duc-startup-detach-user-lisp (file)
  "Replace shared source link FILE with an independent copy for this version."
  (interactive (list (read-file-name "Make version-specific copy: "
                                     user-lisp-directory nil t)))
  (setq file (expand-file-name file))
  (unless (and (string-prefix-p (file-name-as-directory user-lisp-directory) file)
               (string-suffix-p ".el" file)
               (duc-startup--shared-link-p file))
    (user-error "Select a shared .el link inside %s" user-lisp-directory))
  (let ((temporary (make-temp-file (concat file ".copy-"))))
    (unwind-protect
        (progn
          (copy-file file temporary t)
          (rename-file temporary file t)
          (when (file-exists-p (concat file "c")) (delete-file (concat file "c"))))
      (when (file-exists-p temporary) (delete-file temporary))))
  (message "Independent source for Emacs %s: %s" emacs-major-version file))

(defun duc-startup-prepare-lisp ()
  "Compile local Lisp after package declarations load.
Emacs 31 also generates autoloads and queues native compilation.
Emacs 30 uses byte compilation and its normal native compilation cache."
  (interactive)
  (duc-startup-sync-user-lisp)
  (if (fboundp 'prepare-user-lisp)
      (prepare-user-lisp)
    (require 'bytecomp)
    (dolist (file (directory-files-recursively user-lisp-directory "\\.el\\'"))
      (byte-recompile-file file nil 0))))

(provide 'duc-startup)
;; Local Variables:
;; no-byte-compile: t
;; End:
