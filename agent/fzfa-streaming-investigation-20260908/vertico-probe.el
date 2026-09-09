;;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'json)
(require 'vertico)
(require 'fzfa)
(fzf-native-ensure-loaded)
(vertico-mode 1)
(setq inhibit-startup-screen t)

(let* ((mode (or (getenv "PROBE_MODE") "paused"))
       (query (or (getenv "PROBE_QUERY") ""))
       (output-file (getenv "PROBE_OUTPUT"))
       (command (format "/usr/bin/python3 -u %s %s"
                        (shell-quote-argument
                         (expand-file-name "producer.py"
                                           (file-name-directory load-file-name)))
                        (shell-quote-argument mode)))
       (started (float-time))
       timer samples error-data)
  (unwind-protect
      (progn
        (setq timer
              (run-at-time
               0.02 0.025
               (lambda ()
                 (when-let* ((win (active-minibuffer-window)))
                   (with-current-buffer (window-buffer win)
                     (when-let* ((session fzfa--minibuffer-session)
                                 (source (aref (fzfa-session-sources session) 0))
                                 (handle (fzfa-source-handle source)))
                       (let ((status (fzf-native-async-status handle)))
                         (push `((seconds . ,(- (float-time) started))
                                 (reader_done . ,(if (plist-get status :reader-done)
                                                    t :json-false))
                                 (state . ,(plist-get status :state))
                                 (stale . ,(if (plist-get status :stale) t :json-false))
                                 (pool . ,(plist-get status :pool-generation))
                                 (result_pool . ,(plist-get status :result-pool-generation))
                                 (vertico_candidates . ,(length vertico--candidates))
                                 (vertico_total . ,vertico--total)
                                 (source_candidates . ,(length (fzfa-source-last-result source))))
                               samples)))
                     (when (> (- (float-time) started) 5)
                       (abort-recursive-edit)))))))
        (condition-case err
            (fzfa-completing-read :prompt "Streaming probe: "
                                  :command command
                                  :initial-input query)
          (quit nil)
          (error (setq error-data (error-message-string err)))))
    (when timer (cancel-timer timer))
    (with-temp-file output-file
      (insert (json-encode
               `((mode . ,mode) (query . ,query) (error . ,error-data)
                 (samples . ,(vconcat (nreverse samples)))))))))
(kill-emacs 0)
