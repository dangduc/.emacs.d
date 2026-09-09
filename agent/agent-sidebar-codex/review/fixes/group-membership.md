# Group membership and ambiguity fixes

Implemented Ousterhout finding O4 and performance finding LUU-R2-1 in
`agent-sidebar--new-context-for-group`.

The resolver captures the filter at command start. After reading each member's
metadata, it verifies the original breadcrumb and filter again. A changed
repository, model, or filter match raises an error before any launch. Direct row
`N` still uses newly validated metadata, including a changed working directory.

The resolver now stops when a second distinct context proves ambiguity. Additional
entries cannot turn two contexts into one. Membership validation runs before this
ambiguity check. A warm group still reuses cached metadata and avoids discovery.

## Verification

- `group-membership-tests.log`: 7/7 pass, including all three Ousterhout Round 2
  probes, changed filter/model membership, bounded ambiguity rejection, and a
  warm-cache control.
- The cold ambiguity test discovers 20 alternating-directory rollouts. A parser
  call after the second would fail the test. The resolver rejects after two calls.
- `group-membership-workflow-regressions.log`: 12/12 earlier workflow tests pass,
  including direct-row changed-CWD behavior, mixed providers, and mixed homes.

The original Ousterhout and Luu diagnostic logs remain unchanged. Luu's original
cold probe intentionally asserts excessive baseline work; this fix adds a desired
invariant instead of rewriting that historical diagnostic.

## Replay

From `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-sidebar-codex/review/run-review.py \
  --front lisp/agent-sidebar.el --core lisp/agent-shell-sidebar.el \
  --probe agent/agent-sidebar-codex/review/fixes/group-membership-tests.el \
  --selector '(or "^fix-group-" "^ousterhout-r2-")' \
  --log agent/agent-sidebar-codex/review/fixes/group-membership-replay.log
```

No running Emacs, Desktop copy, or compiled library was changed by this assignment.
