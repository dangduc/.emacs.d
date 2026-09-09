# Integration command probe

## I1 — P1: New session on an unparsed row uses the ambient project

The new-session command reads frozen row metadata without ensuring that the
selected entry has been parsed (`baseline-0-agent-sidebar.el:933–941`). Codex
listers do not know the working directory until the JSONL parser runs. If the
user presses `N` on a pending row, the command falls back to the context root.

The local fixture puts the rollout in a different existing working directory,
refreshes the sidebar without draining idle parsing, selects its visible row,
and captures the terminal launch arguments. The command launches in the context
root instead of the recorded directory. This can put subsequent agent work in
the wrong repository.

`root-integration-tests.el` asserts the recorded directory and fails on the
baseline. `root-integration-baseline.log` records both paths. This is a command
integration probe, not a claim that a model was started or modified files.

Replay from the workspace root:

```sh
python3 agent/agent-sidebar-codex/review/run-review.py \
  --probe agent/agent-sidebar-codex/review/root-integration-tests.el \
  --selector review-root-new-session-pending-row-uses-recorded-directory \
  --log agent/agent-sidebar-codex/review/root-integration-replay.log
```

The workflow reviewer will assess related command behavior. The fix must resolve
the current entry and read required metadata before choosing its directory; a
missing or unreadable row must not silently select another project.
