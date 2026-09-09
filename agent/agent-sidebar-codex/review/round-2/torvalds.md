# Round 2 — process ownership and ACP configuration

This is a Linus Torvalds-inspired review perspective, not a claim of participation. It reviews the immutable Round 2 source pair and cross-checks another reviewer's ACP changes.

## R2-T1 — P2: display failure leaves newly launched terminals untracked

Source: `round-2-agent-sidebar.el:554–556`, the final display call in `agent-sidebar--launch-terminal`. `agent-sidebar--visit-terminal` records its identity only afterward, at lines 576–581.

The startup cleanup now covers terminal creation and mode setup. It does not cover displaying the terminal. A display error therefore reports a failed activation while leaving the process running. Its session identity and transcript ownership have not been installed, so retrying starts another process.

The new probe `torvalds-r2-display-error-does-not-leave-unowned-terminal-children` calls the real resume dispatcher and starts actual Emacs `term` children. It injects an error at the display boundary, then retries once. Observation:

```text
DISPLAY-FAILURE: failed activations=2, live children=2, session markers=(nil nil)
```

The assertion that a failed activation leaves no unowned child fails. The child command only executes local `sleep`. The probe kills both processes afterward.

This is a reproduced exception path through fault injection. It does not establish how often a user's display configuration raises an error. A suitable fix is to include display in the cleanup transaction for newly created terminals, or preserve the started process and install its identity before attempting display. An existing buffer returned by a custom terminal function must not be destroyed as if the sidebar had created it.

## Passing control: separate wrapped configs retain their homes at the OS boundary

The new `torvalds-r2-interleaved-wrappers-preserve-home-at-os-boundary` test builds a wrapped configuration for home A, then rewraps it for home B. It changes ambient `CODEX_HOME` before constructing either client. Both clients use the real ACP process launcher and a local Python recorder.

Both child processes receive their own selected home and working directory. The unrelated `SIDEBAR_REVIEW_TAG` environment variable remains present. The base configuration's environment remains unchanged, and unwrapping the second configuration returns the original base rather than a nested wrapper.

Only the temporary home, working directory, and a synthetic marker are recorded. No authentication values are inspected or written to the log. No provider, network session, or model prompt is used.

## Replay

```sh
python3 /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/run-review.py \
  --front /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-2-agent-sidebar.el \
  --core /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-2-agent-shell-sidebar.el \
  --probe /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-2/torvalds-tests.el \
  --log /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-2/torvalds.log
```

Result: 2 new tests; the ACP control passes, and the terminal display-failure assertion reproduces R2-T1. No product changes were made in this review.
