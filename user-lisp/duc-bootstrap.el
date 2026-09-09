;;; duc-bootstrap.el --- Package.el bootstrap workers -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'package)
(require 'lisp-mode)
(require 'duc-startup)

(defvar duc-bootstrap-concurrency 8
  "Maximum number of package or grammar workers per run.")
(defvar duc-bootstrap-worker-timeout 600
  "Maximum lifetime of a worker, in seconds.")

(defun duc-bootstrap--keyword (keyword body)
  "Return the tail at KEYWORD in a use-package BODY.
Unlike a plist, use-package keywords can have several values."
  (memq keyword body))

(defun duc-bootstrap--package (form)
  "Return the archive package requested by use-package FORM, or nil."
  (let* ((body (cddr form))
         (ensure (duc-bootstrap--keyword :ensure body)))
    (unless (or (and ensure (null (cadr ensure)))
                (duc-bootstrap--keyword :vc body)
                (cadr (duc-bootstrap--keyword :disabled body))
                (cl-some
                 (lambda (key)
                   (let ((cell (duc-bootstrap--keyword key body)))
                     (and cell (not (eval (cadr cell) t)))))
                 '(:if :when))
                (let ((cell (duc-bootstrap--keyword :unless body)))
                  (and cell (eval (cadr cell) t))))
      (let ((pkg (if (and ensure (not (eq (cadr ensure) t)))
                     (cadr ensure)
                   (cadr form))))
        (unless (symbolp pkg)
          (error "Unsupported :ensure value in %S" form))
        (unless (or (package-built-in-p pkg)
                    (duc-startup-local-library-p pkg))
          pkg)))))

(defun duc-bootstrap--contains-declaration-p (form)
  "Return non-nil for a supported FORM that contains use-package."
  (and (consp form)
       (or (eq (car form) 'use-package)
           (and (memq (car form) '(progn eval-and-compile eval-when-compile
                                         when unless if))
                (cl-some #'duc-bootstrap--contains-declaration-p (cdr form))))))

(defun duc-bootstrap--collect-form (form)
  "Collect package names from a declarative FORM.
Read trusted configuration only.  Do not scan quoted examples or functions."
  (when (duc-bootstrap--contains-declaration-p form)
    (pcase (car form)
      ('use-package (when-let* ((pkg (duc-bootstrap--package form))) (list pkg)))
      ((or 'progn 'eval-and-compile 'eval-when-compile)
       (mapcan #'duc-bootstrap--collect-form (cdr form)))
      ('when (when (eval (cadr form) t)
               (mapcan #'duc-bootstrap--collect-form (cddr form))))
      ('unless (unless (eval (cadr form) t)
                 (mapcan #'duc-bootstrap--collect-form (cddr form))))
      ('if (if (eval (cadr form) t)
               (duc-bootstrap--collect-form (nth 2 form))
             (mapcan #'duc-bootstrap--collect-form (nthcdr 3 form)))))))

(defun duc-bootstrap-collect-packages ()
  "Read archive package declarations from this version's effective user Lisp."
  (let (packages)
    (dolist (file (directory-files-recursively
                   user-lisp-directory "\\.el\\'"))
      (with-temp-buffer
        (set-syntax-table emacs-lisp-mode-syntax-table)
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case err
            (while (progn (forward-comment (point-max)) (not (eobp)))
              (setq packages
                    (nconc packages (duc-bootstrap--collect-form
                                     (read (current-buffer))))))
          (error (error "Cannot read package declarations in %s: %s"
                        file (error-message-string err))))))
    (delete-dups packages)))

(defun duc-bootstrap-ensure-package (name args state &optional no-refresh)
  "Install archive packages in ARGS unless local sources provide them.
Preserve use-package's handling of NAME, STATE, and NO-REFRESH."
  (use-package-ensure-elpa
   name
   (cl-remove-if
    (lambda (ensure)
      (let ((package (if (eq ensure t) name ensure)))
        (when (consp package) (setq package (car package)))
        (and package (duc-startup-local-library-p package))))
    args)
   state no-refresh))

(cl-defstruct (duc-bootstrap-run (:constructor duc-bootstrap--make-run))
              pending processes failures (completed 0) total concurrency callback)

(defun duc-bootstrap--dispatch (run)
  "Start available workers for RUN."
  (while (and (duc-bootstrap-run-pending run)
              (< (length (duc-bootstrap-run-processes run))
                 (duc-bootstrap-run-concurrency run)))
    (pcase-let* ((`(,name . ,form) (pop (duc-bootstrap-run-pending run)))
                 (buffer (get-buffer-create (format "*duc worker: %s*" name))))
      (with-current-buffer buffer (erase-buffer))
      (condition-case err
          (let* ((process-connection-type nil)
                 (process
                  (make-process
                   :name (format "duc:%s" name) :buffer buffer
                   :noquery t :connection-type 'pipe
                   :command (list (expand-file-name invocation-name invocation-directory)
                                  "--batch" "-Q" "--eval"
                                  (let ((print-length nil) (print-level nil))
                                    (prin1-to-string form)))
                   :sentinel
                   (lambda (proc _event)
                     (when (and (memq (process-status proc) '(exit signal))
                                (not (process-get proc 'duc-finished)))
                       (process-put proc 'duc-finished t)
                       (when-let* ((timer (process-get proc 'duc-timeout)))
                         (cancel-timer timer))
                       (setf (duc-bootstrap-run-processes run)
                             (delq proc (duc-bootstrap-run-processes run)))
                       (unless (zerop (process-exit-status proc))
                         (push name (duc-bootstrap-run-failures run)))
                       (cl-incf (duc-bootstrap-run-completed run))
                       (message "[duc] %s: %s (%s/%s)"
                                name (if (zerop (process-exit-status proc)) "done" "FAILED")
                                (duc-bootstrap-run-completed run)
                                (duc-bootstrap-run-total run))
                       (duc-bootstrap--dispatch run))))))
            (push process (duc-bootstrap-run-processes run))
            (process-put process 'duc-timeout
                         (run-at-time duc-bootstrap-worker-timeout nil
                                      (lambda ()
                                        (when (process-live-p process)
                                          (delete-process process))))))
        (error
         (with-current-buffer buffer (insert (error-message-string err)))
         (push name (duc-bootstrap-run-failures run))
         (cl-incf (duc-bootstrap-run-completed run))))))
  (when (and (null (duc-bootstrap-run-pending run))
             (null (duc-bootstrap-run-processes run))
             (duc-bootstrap-run-callback run))
    (let ((callback (duc-bootstrap-run-callback run)))
      (setf (duc-bootstrap-run-callback run) nil)
      (funcall callback run))))

(defun duc-bootstrap-run-jobs (jobs &optional concurrency callback)
  "Run JOBS, a list of (NAME . FORM), in this Emacs executable.
CONCURRENCY bounds the pool.  CALLBACK receives the completed run.
Each worker retains its output in a *duc worker: NAME* buffer."
  (let ((limit (or concurrency duc-bootstrap-concurrency)))
    (unless (and (integerp limit) (> limit 0))
      (error "Concurrency must be a positive integer"))
    (let ((run (duc-bootstrap--make-run
                :pending (copy-sequence jobs) :total (length jobs)
                :concurrency limit :callback callback)))
      (duc-bootstrap--dispatch run)
      run)))

(defun duc-bootstrap-wait (run)
  "Wait for RUN and report failed workers.  Cancel workers on quit."
  (unwind-protect
      (progn
        (while (or (duc-bootstrap-run-pending run)
                   (duc-bootstrap-run-processes run))
          (accept-process-output nil 0.1))
        (when (duc-bootstrap-run-failures run)
          (error "Workers failed: %s. See *duc worker: NAME* buffers"
                 (duc-bootstrap-run-failures run))))
    (setf (duc-bootstrap-run-pending run) nil
          (duc-bootstrap-run-callback run) nil)
    (dolist (proc (copy-sequence (duc-bootstrap-run-processes run)))
      (when (process-live-p proc) (delete-process proc)))))

(defun duc-bootstrap--install-job (desc)
  "Return a worker job that installs exactly DESC, without its dependencies."
  (cons
   (package-desc-name desc)
   `(progn
      (require 'package)
      (setq user-emacs-directory ,user-emacs-directory
            package-user-dir ,package-user-dir
            package-archives ',package-archives
            package-archive-priorities ',package-archive-priorities
            package-check-signature ',package-check-signature
            package-unsigned-archives ',package-unsigned-archives
            package-gnupghome-dir ,package-gnupghome-dir
            package-quickstart nil
            package-quickstart-file (make-temp-name
                                     (expand-file-name "duc-worker-quickstart-"
                                                       temporary-file-directory))
            package-native-compile nil
            native-comp-jit-compilation nil
            native-comp-deferred-compilation nil)
      (setq native-comp-eln-load-path ',(when (boundp 'native-comp-eln-load-path)
                                          native-comp-eln-load-path))
      (package-initialize)
      ;; The parent dispatches this job only after its dependencies finish.
      ;; package-install would recursively install them and race other workers.
      (package-install-from-archive ',desc)
      (unless (package-installed-p ',(package-desc-name desc)
                                   ',(package-desc-version desc))
        (error "Package installation did not produce %s" ',(package-desc-name desc))))))

(defun duc-bootstrap--install-transaction (transaction concurrency)
  "Install TRANSACTION in dependency waves with at most CONCURRENCY workers."
  (let ((pending transaction) completed)
    (while pending
      (let ((ready
             (cl-remove-if-not
              (lambda (desc)
                (cl-every
                 (lambda (requirement)
                   (let* ((name (car requirement))
                          (dependency
                           (cl-find name transaction :key #'package-desc-name)))
                     (if dependency (memq name completed)
                       (package-installed-p name (cadr requirement)))))
                 (package-desc-reqs desc)))
              pending)))
        (unless ready
          (error "Unresolved package dependencies: %s"
                 (mapcar #'package-desc-name pending)))
        (duc-bootstrap-wait
         (duc-bootstrap-run-jobs (mapcar #'duc-bootstrap--install-job ready) concurrency))
        (setq completed (nconc (mapcar #'package-desc-name ready) completed)
              pending (cl-set-difference pending ready))))))

(defun duc-bootstrap-parallel-install (&optional concurrency)
  "Install missing archive packages with dependency-aware workers.
VC declarations remain under use-package control.  Installed packages stay
at their current versions unless a missing package needs a newer dependency."
  (interactive)
  (let ((packages (cl-remove-if #'package-installed-p
                                (duc-bootstrap-collect-packages))))
    (when packages
      (let ((lock (expand-file-name ".duc-bootstrap-lock" package-user-dir)))
        ;; An atomic directory creation excludes another bootstrap of this major.
        (condition-case nil (make-directory lock)
          (file-already-exists
           (error "Bootstrap lock exists: %s. Check for an active bootstrap" lock)))
        (unwind-protect
            (progn
              (write-region (format "pid=%s\n" (emacs-pid)) nil
                            (expand-file-name "owner" lock) nil 'silent)
              (package-read-all-archive-contents)
              (unless (and package-archive-contents
                           (cl-every (lambda (pkg) (assq pkg package-archive-contents))
                                     packages))
                (package-refresh-contents))
              (duc-bootstrap--install-transaction
               (package-compute-transaction nil (mapcar #'list packages))
               concurrency)
              (package-initialize)
              (when package-quickstart (package-quickstart-refresh)))
          (delete-directory lock t))))))

(defun duc-bootstrap-wait-for-native-comp (&optional timeout)
  "Wait at most TIMEOUT seconds for native compilation.  Do not restart."
  (interactive)
  (require 'comp-run nil t)
  (let ((deadline (+ (float-time) (or timeout 300))))
    (while (or (bound-and-true-p comp-files-queue)
               (and (boundp 'comp-async-compilations)
                    (cl-loop for process being each hash-value
                             of comp-async-compilations
                             thereis (process-live-p process))))
      (when (> (float-time) deadline)
        (error "Native compilation did not finish before the timeout"))
      (accept-process-output nil 0.1)))
  (message "Native compilation is complete"))

(provide 'duc-bootstrap)
;;; duc-bootstrap.el ends here
