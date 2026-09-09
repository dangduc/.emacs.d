# Delegated ACP configuration fixes

Addressed O1, O2, and O3 from the Round 1 Ousterhout-inspired review.

The frontend now wraps the selected Codex configuration for each session. The wrapper replaces only `CODEX_HOME` in each client's explicit environment. This lets the selected store survive delayed client creation and agent-shell reload. Other authentication and environment entries come from the original client maker. The original configuration and returned client environment are not modified.

Configuration fields available to other sidebar lifecycle code:

- `:agent-sidebar-codex-home`: expanded selected path spelling, sent to the child process.
- `:agent-sidebar-codex-base-config`: original configuration, used for reuse comparisons.

`agent-sidebar--codex-home-identity` resolves aliases and normalizes the final directory separator for identity comparisons. `agent-sidebar--codex-buffer-home` returns a known canonical home. It checks the actual client's explicit environment before preserved configuration and legacy sidebar annotations. It does not infer an ordinary agent-shell buffer's home from the current ambient environment.

Reuse now checks all buffers against session ID, working directory, base configuration, and home before selecting a buffer. An active session ID takes precedence over a pending resume ID.

## Verification

The three original failing invariants now pass, including two checks against actual local ACP subprocesses. Six additional controls verify:

- Deferred client creation preserves the selected home and other authentication/environment fields.
- Wrapping does not modify a client environment returned by another owner.
- An ordinary ACP buffer with an explicit matching home can be reused.
- An ordinary buffer with an unknown home is not guessed from ambient settings.
- An actual client home takes precedence over a stale sidebar marker.
- A configuration retained by reload can be reused through an equivalent symlink or final-slash spelling.

Replay from `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-sidebar-codex/review/run-review.py \
  --front lisp/agent-sidebar.el \
  --probe agent/agent-sidebar-codex/review/fixes/acp-tests.el \
  --log agent/agent-sidebar-codex/review/fixes/acp-replay.log
```

Result in `acp.log`: 40 tests passed. This includes the six new controls, three original findings, and 31 existing Codex/merge tests. The original finding-only replay is retained in `acp-original.log`.

Only the frontend's Codex configuration/start/reuse helper region was changed. Desktop copies, running Emacs, compilation outputs, parsing, terminal commands, and ownership behavior were left to their assigned tasks.
