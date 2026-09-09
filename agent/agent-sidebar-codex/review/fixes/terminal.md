# Terminal identity fixes

Implemented R1-T1 and R1-T2 in `lisp/agent-sidebar.el`.

- Codex terminal identity now uses the shared `agent-sidebar--codex-home-identity` helper. Equivalent home spellings reuse the same session; different homes remain isolated.
- The `term` fallback reserves an unused buffer name before `make-term`. A new session and a resume label collision therefore start new processes.
- A failed or cancelled term startup deletes only its newly allocated buffer and process. Existing buffers remain intact, and cleanup does not ask a kill-buffer question.
- `make-term` sets term mode itself. The launcher now only enables character mode afterward.

The ghostel, vterm, custom-runner, and ACP implementation branches were not changed by this fix.

## Validation

13 targeted tests passed on the current source. These include all three Round 1 terminal scenarios, three startup-cleanup cases, real term argument and working-directory checks, intentional reuse, distinct Codex homes, ghostel dispatch and failure cleanup, vterm side-window handling, and active-file ownership.

The original public `N` probe used only a text-property row. Another review fix now requires current entry membership and parsed metadata. `terminal-tests.el` supplies that valid fixture while keeping the original assertion. Running the three scenarios against the original baseline still produces all three intended failures; see `terminal-baseline-controls.log`.

Replay:

```sh
python3 /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/run-review.py \
  --front /Users/ducnguyen/.emacs.d/lisp/agent-sidebar.el \
  --core /Users/ducnguyen/.emacs.d/lisp/agent-shell-sidebar.el \
  --probe /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/fixes/terminal-tests.el \
  --log /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/fixes/terminal-tests.log \
  --selector '(or "^torvalds-" "^terminal-fix-" "^merge-terminal-" "^merge-term-" "^merge-real-term-" "^merge-vterm-" "^merge-ghostel-" "^codex-cli-ghostel-")'
```

Native ghostel rendering was not repeated in this delegated fix. The dispatch and process contract tests passed; the parent task retains the running-Emacs integration check. No Desktop files, compiled files, or running Emacs state were changed here.
