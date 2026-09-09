# Terminal display-failure cleanup

Implemented R2-T1 in `agent-sidebar--launch-terminal` only.

The launcher records which buffers existed before startup. If displaying the returned terminal fails or is cancelled, it deletes the newly created child and buffer, suppresses kill queries, and signals the original error or quit. A buffer that existed before the call remains under its previous owner's control. Intentional session reuse still bypasses the launcher.

The original failing scenario now reports:

```text
DISPLAY-FAILURE: failed activations=2, live children=0, session markers=(nil nil)
```

19 targeted tests passed. Coverage includes the Round 2 real-process invariant, exact error and quit propagation, cleanup of new custom and ghostel buffers, preservation of a custom runner's existing buffer/process/content, preservation during intentional reuse, and the earlier terminal, argument, working-directory, home-identity, ownership, and ACP controls.

The four new display controls were also run against the immutable Round 2 baseline. Its two cleanup cases failed; its two existing-buffer preservation controls passed. See `display-baseline-controls.log`. The new native ghostel test uses its dispatch contract with a local `cat` process; it does not exercise rendering.

Replay:

```sh
python3 /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/run-review.py \
  --front /Users/ducnguyen/.emacs.d/lisp/agent-sidebar.el \
  --core /Users/ducnguyen/.emacs.d/lisp/agent-shell-sidebar.el \
  --probe /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/fixes/display-tests.el \
  --log /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/fixes/display-tests.log \
  --selector '(or "^display-fix-" "^torvalds-" "^terminal-fix-" "^merge-terminal-" "^merge-term-" "^merge-real-term-" "^merge-vterm-" "^merge-ghostel-" "^codex-cli-ghostel-")'
```

No Desktop files, compiled files, or running Emacs state were changed in this delegated fix.
