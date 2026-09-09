# Command context and activation fixes

Implemented review findings CW1/I1, CW2, and K2 in `lisp/agent-sidebar.el`.
Only activation validation, current-row resolution, group context, and new-session
commands were changed by this fix assignment.

## Resulting behavior

`N` resolves the current entry and reads its required metadata before selecting
a working directory. An unreadable Codex row or a row without an existing CWD
raises a user error before starting a process. A replaced CWD is read again.

Groups derive their provider, agent, directory, and Codex home from the current
entries represented by their breadcrumb and filter. One shared context permits
`N`; multiple contexts require selection of a transcript row. Removing `package`
from grouping no longer changes a unique Codex group's backend. A prefix argument
keeps Codex agent-shell selection and the entries' recorded Codex home.

The metadata activation helper checks its owner, generation, current entry,
provider registration, and cache after a read. The core already refuses an obsolete
cache write, but it returns the obsolete metadata to its caller. The frontend now
refuses to launch from that returned metadata. A refresh during a read requests a
retry, even when the file path remains present: discovery may replace its provider
or Codex home without changing the file signature.

A blank-area `N` retains the existing generic agent-shell/context-root workflow.
The stricter checks apply when a row or group supplies the action context.

## Evidence

All commands ran through `review/run-review.py` against the working frontend and
core source. All process launch assertions use local recorders, except the existing
31-test suite's already established offline terminal and ACP fixtures.

- `workflow-tests.log`: 12/12 pass. Includes the five original workflow probes and
  seven added tests for mixed provider/home groups, captured home and prefix use,
  Codex agent-shell groups, removed rows, provider replacement, and owner changes.
- `workflow-root-integration.log`: I1's pending-row CWD probe passes.
- `workflow-kingsbury-removal.log`: K2 passes; a row removed by reentrant refresh
  produces zero launches.
- `workflow-existing-tests.log`: existing 31/31 source tests pass.

The first combined loader attempted to reload shared ERT definitions and stopped
before running tests. Its output is retained as `workflow-initial-loader-error.log`.
The corrected runs load shared definitions once or use separate processes.

## Replay

From `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-sidebar-codex/review/run-review.py \
  --front lisp/agent-sidebar.el --core lisp/agent-shell-sidebar.el \
  --probe agent/agent-sidebar-codex/review/fixes/workflow-tests.el \
  --selector '(or "^fix-workflow-" "^review-workflow-")' \
  --log agent/agent-sidebar-codex/review/fixes/workflow-replay.log
```

The original Round 1 logs remain unchanged. No Desktop copy, running Emacs, or
compiled library was changed during this fix assignment.
