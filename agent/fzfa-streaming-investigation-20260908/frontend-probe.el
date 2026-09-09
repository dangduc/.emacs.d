;;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'json)
(require 'fzfa)
(fzf-native-ensure-loaded)
(setq inhibit-startup-screen t)
(defvar probe-frontend (getenv "PROBE_FRONTEND"))
(defvar probe-sources nil)
(defvar probe-ivy-display "")
(defun probe-capture-source (source)
  (push source probe-sources)
  source)
(advice-add 'fzfa-make-source :filter-return #'probe-capture-source)
(pcase probe-frontend
  ("helm"
   (require 'helm)
   (require 'helm-mode)
   (require 'fzfa-helm)
   (helm-mode 1))
  ("ivy"
   (require 'ivy)
   (require 'fzfa-ivy)
   (fzfa-ivy-setup)
   (ivy-mode 1)
   (advice-add 'ivy--insert-minibuffer :after
               (lambda (text) (setq probe-ivy-display text))))
  ("vertico"
   (require 'vertico)
   (vertico-mode 1)))

(let* ((mode (or (getenv "PROBE_MODE") "flood"))
       (layout (or (getenv "PROBE_LAYOUT") "single"))
       (query (or (getenv "PROBE_QUERY") "alpha"))
       (output-file (getenv "PROBE_OUTPUT"))
       (directory (file-name-directory load-file-name))
       (command (format "/usr/bin/python3 -u %s %s"
                        (shell-quote-argument
                         (expand-file-name
                          (if (equal mode "queries")
                              "query-producer.py" "producer.py") directory))
                        (shell-quote-argument mode)))
       (started (float-time))
       timer samples error-data)
  (unwind-protect
      (progn
        (setq timer
              (run-at-time
               0.02 0.025
               (lambda ()
                 (condition-case err
                     (when-let* ((win (active-minibuffer-window))
                                 (source (cl-find-if #'fzfa-source-handle
                                                     probe-sources))
                                 (handle (fzfa-source-handle source)))
                       (with-current-buffer (window-buffer win)
                         (let* ((status (fzf-native-async-status handle))
                                (candidates
                                 (pcase probe-frontend
                                   ("helm"
                                    (when (get-buffer helm-buffer)
                                      (with-current-buffer helm-buffer
                                        (split-string
                                         (buffer-substring-no-properties
                                          (point-min) (point-max)) "\n" t))))
                                   ("ivy" ivy--all-candidates)
                                   ("vertico" vertico--candidates)))
                                (records (cl-remove-if-not
                                          (lambda (candidate)
                                            (string-match-p "doc-[0-9]" candidate))
                                          candidates))
                                (display
                                 (pcase probe-frontend
                                   ("ivy" probe-ivy-display)
                                   (_ (mapconcat #'identity records "\n")))))
                           (push `((seconds . ,(- (float-time) started))
                                   (input . ,(pcase probe-frontend
                                               ("ivy" (ivy--input))
                                               ("helm" helm-pattern)
                                               (_ (minibuffer-contents-no-properties))))
                                   (query . ,(plist-get status :query))
                                   (reader_done . ,(if (plist-get status :reader-done)
                                                      t :json-false))
                                   (state . ,(plist-get status :state))
                                   (stale . ,(if (plist-get status :stale) t :json-false))
                                   (pool . ,(plist-get status :pool-generation))
                                   (result_pool . ,(plist-get status :result-pool-generation))
                                   (source_output . ,(car-safe (fzfa-source-request-output source)))
                                   (candidates . ,(length records))
                                   (display_has_records . ,(if (string-match-p "doc-[0-9]" display)
                                                               t :json-false))
                                   (alpha . ,(cl-count-if
                                              (lambda (c) (string-match-p "alpha" c)) records))
                                   (bravo . ,(cl-count-if
                                              (lambda (c) (string-match-p "bravo" c)) records))
                                   (first . ,(car records)))
                                 samples))
                         (when (> (- (float-time) started) 5)
                           (abort-recursive-edit))))
                   (quit (signal 'quit nil))
                   (error
                    (setq error-data (error-message-string err))
                    (abort-recursive-edit))))))
        (condition-case err
            (if (equal layout "multi")
                (fzfa--read
                 (list (list :name "stream" :command command
                             :initial-input query :action #'identity)
                       (list :name "static" :candidates '("static alpha" "static bravo")
                             :action #'identity))
                 :prompt "Streaming probe: ")
              (fzfa-completing-read :prompt "Streaming probe: "
                                    :command command
                                    :initial-input query))
          (quit nil)
          (error (setq error-data (error-message-string err)))))
    (when timer (cancel-timer timer))
    (with-temp-file output-file
      (insert (json-encode
               `((frontend . ,probe-frontend) (mode . ,mode) (query . ,query)
                 (layout . ,layout)
                 (core_file . ,(symbol-file 'fzfa--source-async-out 'defun))
                 (error . ,error-data)
                 (samples . ,(vconcat (nreverse samples)))))))))
(kill-emacs 0)
