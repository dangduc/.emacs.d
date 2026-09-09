;;; ousterhout-tests.el --- Group context boundary review -*- lexical-binding: t; -*-
(load "/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/tests.el" nil t)

(ert-deftest ousterhout-r2-new-on-stale-repo-group-does-not-launch-another-repo ()
  "A selected repo group must not silently become a different repo during parsing."
  (codex-test--fixture
    (let* ((file (codex-test--write agent-sidebar-codex-home root))
           (new-root (expand-file-name "changed-repository-longer-name/" temporary))
           launch result)
      (make-directory new-root)
      (agent-sidebar-refresh) (merge--drain)
      (agent-sidebar-set-grouping '(repo))
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'agent-sidebar-group) (list (cons 'repo root))))
      ;; The displayed group remains A.  The transcript now belongs to B.
      (codex-test--write agent-sidebar-codex-home new-root)
      (should (equal file (plist-get (car agent-sidebar--entry-list) :id)))
      (cl-letf (((symbol-function 'agent-sidebar--require-codex-terminal) #'ignore)
                ((symbol-function 'agent-sidebar--launch-terminal)
                 (lambda (_name _program _args directory) (setq launch directory))))
        (setq result (condition-case error (agent-sidebar-new-session) (user-error error))))
      (message "Stale group: selected=%s current-cwd=%s launched=%S result=%S"
               root new-root launch result)
      (should-not launch)
      (should (eq (car-safe result) 'user-error)))))

(defun ousterhout-r2--filtered-group-launch (&optional ignore-filter)
  "Return a launch tuple for a filtered group; optionally disable membership filtering."
  (codex-test--fixture
    (codex-test--write agent-sidebar-codex-home root)
    (merge--json-file root store "claude")
    (agent-sidebar-refresh) (merge--drain)
    (agent-sidebar-set-grouping '(repo))
    (agent-sidebar-set-filter "Native Codex prompt")
    (goto-char (point-min))
    (should (= (length (merge--rows)) 1))
    (let (launch)
      (cl-letf (((symbol-function 'agent-sidebar--require-codex-terminal) #'ignore)
                ((symbol-function 'agent-sidebar--launch-terminal)
                 (lambda (name _program _args directory) (setq launch (list name directory))))
                ((symbol-function 'agent-shell-sidebar--start)
                 (lambda (&rest _) (ert-fail "Wrong agent-shell backend"))))
        (if ignore-filter
            (cl-letf (((symbol-function 'agent-sidebar--matches-filter-p) (lambda (_entry) t)))
              (agent-sidebar-new-session))
          (agent-sidebar-new-session)))
      (should (equal launch (list "*codex-new*" root)))
      launch)))

(ert-deftest ousterhout-r2-filtered-group-resolves-visible-provider ()
  "Hidden Claude rows must not make a uniquely visible Codex group ambiguous."
  (should (ousterhout-r2--filtered-group-launch)))

(ert-deftest ousterhout-r2-filter-membership-mutation-is-detected ()
  "Disabling the filter at context selection must break the visible-group control."
  (should-error (ousterhout-r2--filtered-group-launch t) :type 'user-error)
  (message "Mutation control: ignoring membership filter produced ambiguity, as detected"))
