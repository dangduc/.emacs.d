# Round 2: Contrarian workflow perspective

This review uses a workflow skeptic perspective. The immutable sources are
`round-2-agent-sidebar.el` and `round-2-agent-shell-sidebar.el`.
Five new executable probes ran: four passed and one desired invariant failed.
No external session, prompt, or remote filesystem request was issued.

## CW3 — P2: RET probes a remote CWD before rejecting local-only startup

`agent-sidebar--visit-terminal` checks `file-directory-p` at snapshot line 574
without first rejecting a remote path. Its eventual terminal launcher rejects
remote directories, but that guard runs after this filesystem probe.

The fixture stores `/ssh:sidebar-review.invalid:/repo/` as the CWD in a local
Codex rollout. It disables all file handlers, models remote-path classification,
and replaces directory checks with a recorder. Calling the real `RET` command
records one directory check on the remote path. Calling `A` and `N` records none;
both reject the CWD before testing its existence.

With real remote handlers enabled, `file-directory-p` delegates such a path to
its handler. That can require a connection or authentication before the command
reaches its local-directory error. The fixture establishes the ordering bug;
it does not establish a particular remote latency or authentication behavior.

Reject remote CWDs in the shared terminal resume validation before calling
`file-directory-p`. This affects both native Codex and Claude CLI sessions.

Failing test: `workflow-r2-local-codex-actions-reject-remote-cwd-before-stat`.
The log includes separate RET, A, and N observations.

## Passing checks

- Collapsing a Codex repository group preserves its new-session backend and CWD.
- A repository group containing Codex and Claude agent-shell sessions rejects
  ambiguous agent selection, even though the provider and CWD are identical.
- A mutation that removes `:agent` from action contexts makes that ambiguous
  group start a session. The test detects this difference.
- The derived mode exposes RET/A/N and retains the shared transcript-opening
  key and parser/ownership callbacks.

Ousterhout finding O4 is already recorded separately. It is not counted again
here. The delegated O4 fix follows completion of this immutable-source review.

## Replay

From `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-sidebar-codex/review/run-review.py \
  --front agent/agent-sidebar-codex/review/round-2-agent-sidebar.el \
  --core agent/agent-sidebar-codex/review/round-2-agent-shell-sidebar.el \
  --probe agent/agent-sidebar-codex/review/round-2/contrarian-workflow-tests.el \
  --selector '"^workflow-r2-"' \
  --log agent/agent-sidebar-codex/review/round-2/contrarian-workflow-replay.log
```

The original output is `contrarian-workflow.log`, exit status 1: four passes,
one failure. The launch functions in these checks only record local arguments.
