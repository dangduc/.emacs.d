;;; acp-tests.el --- Per-session Codex configuration controls -*- lexical-binding: t; -*-
(load "/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-1/ousterhout-tests.el" nil t)

(ert-deftest acp-fix-deferred-client-keeps-home-and-other-environment ()
  (codex-test--fixture
    (let* ((agent-shell-openai-authentication (agent-shell-openai-make-authentication :login t))
           (other-env '("CODEX_HOME=/other" "KEEP_THIS=fixture" "CODEX_HOME"))
           (agent-shell-openai-codex-environment other-env)
           (config (agent-shell-openai-make-codex-config))
           (original-config (copy-tree config))
           (original-auth (copy-tree agent-shell-openai-authentication))
           (selected-spelling (directory-file-name agent-sidebar-codex-home))
           (wrapped (agent-sidebar--codex-session-config config selected-spelling)))
      ;; Client construction occurs after the sidebar's dynamic scope ends.
      (let* ((agent-sidebar-codex-home "/a-new-sidebar-selection")
             (process-environment (cons "CODEX_HOME=/ambient" process-environment))
             (client (funcall (map-elt wrapped :client-maker) buffer))
             (env (map-elt client :environment-variables)))
        (should (equal (seq-filter (lambda (v) (string-prefix-p "CODEX_HOME" v)) env)
                       (list (concat "CODEX_HOME=" selected-spelling))))
        (should (member "KEEP_THIS=fixture" env))
        (should (member "OPENAI_API_KEY=" env))
        (should (seq-some (lambda (v) (string-prefix-p "DEFAULT_AUTH_REQUEST=" v)) env)))
      (should (equal config original-config))
      (should (eq agent-shell-openai-codex-environment other-env))
      (should (equal agent-shell-openai-authentication original-auth))
      (should (equal (map-elt wrapped :agent-sidebar-codex-home) selected-spelling))
      (should (eq (agent-sidebar--codex-base-config wrapped) config)))))

(ert-deftest acp-fix-wrapper-does-not-mutate-shared-client-environment ()
  (codex-test--fixture
    (let* ((original-env '("CODEX_HOME=/wrong" "EXTRA=retained"))
           (original-client `((:command . "/bin/cat") (:environment-variables . ,original-env)))
           (config `((:client-maker . ,(lambda (_buffer) original-client))))
           (wrapped (agent-sidebar--codex-session-config config agent-sidebar-codex-home))
           (client (funcall (map-elt wrapped :client-maker) buffer)))
      (should (equal (map-elt original-client :environment-variables) original-env))
      (should (equal (map-elt client :environment-variables)
                     (list (concat "CODEX_HOME=" agent-sidebar-codex-home) "EXTRA=retained"))))))

(defmacro acp-fix--candidate (&rest body)
  (declare (indent 0))
  `(codex-test--fixture
     (let* ((config (agent-shell-openai-make-codex-config))
            (agent-shell-agent-configs (list config))
            (candidate (generate-new-buffer " *ACP candidate*"))
            (replacement (generate-new-buffer " *ACP replacement*"))
            (starts 0))
       (unwind-protect
           (progn
             (with-current-buffer candidate
               (setq-local default-directory root)
               (setq-local agent-shell--state
                           `((:session . ((:id . ,codex-test--id))) (:agent-config . ,config))))
             (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list candidate)))
                       ((symbol-function 'agent-shell-sidebar--start)
                        (lambda (&rest _) (cl-incf starts) replacement)))
               ,@body))
         (dolist (shell (list candidate replacement)) (kill-buffer shell))))))

(ert-deftest acp-fix-ordinary-client-with-explicit-home-is-reused ()
  (acp-fix--candidate
    (with-current-buffer candidate
      (setf (alist-get :client agent-shell--state)
            `((:environment-variables . (,(concat "CODEX_HOME=" agent-sidebar-codex-home))))))
    (should (eq (agent-sidebar--codex-start-agent-shell root codex-test--id) candidate))
    (should (= starts 0))))

(ert-deftest acp-fix-ordinary-client-with-unknown-home-is-not-guessed ()
  (acp-fix--candidate
    (let ((process-environment (cons (concat "CODEX_HOME=" agent-sidebar-codex-home) process-environment)))
      (should (eq (agent-sidebar--codex-start-agent-shell root codex-test--id) replacement)))
    (should (= starts 1))))

(ert-deftest acp-fix-actual-client-home-wins-over-stale-marker ()
  (acp-fix--candidate
    (with-current-buffer candidate
      (setq-local agent-sidebar--codex-session-home agent-sidebar-codex-home)
      (setf (alist-get :client agent-shell--state)
            '((:environment-variables . ("CODEX_HOME=/other-store")))))
    (should (eq (agent-sidebar--codex-start-agent-shell root codex-test--id) replacement))
    (should (= starts 1))))

(ert-deftest acp-fix-reloaded-config-and-home-alias-are-reused ()
  (acp-fix--candidate
    (let ((alias (expand-file-name "codex-alias" temporary)))
      (make-symbolic-link agent-sidebar-codex-home alias)
      (with-current-buffer candidate
        ;; A reloaded buffer has the wrapper but no old buffer-local marker.
        (should-not agent-sidebar--codex-session-home)
        (setf (alist-get :agent-config agent-shell--state)
              (agent-sidebar--codex-session-config config alias)))
      (let ((agent-sidebar-codex-home (directory-file-name agent-sidebar-codex-home)))
        (should (eq (agent-sidebar--codex-start-agent-shell root codex-test--id) candidate)))
      (should (= starts 0)))))
