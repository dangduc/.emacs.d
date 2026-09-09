# Round 1: Contrarian workflow perspective

This review tests actions taken from incomplete rows and alternative group layouts.
It uses a workflow skeptic perspective, not a claim that an outside reviewer participated.

The reviewed files are `baseline-0-agent-sidebar.el` and `baseline-1-agent-shell-sidebar.el`.
Five isolated ERT probes ran. Four desired invariants failed; one control passed.
Launch functions were replaced with argument recorders. No agent prompt was sent.

## CW1 — P1: New-session actions trust missing or stale row metadata

This extends and duplicates integration finding **I1**. Count it as one finding family.

`agent-sidebar-new-session` reads the rendered entry at line 933. Unlike `RET` and
`A`, it does not resolve the current entry or call `agent-sidebar--ensure-parsed`.
Lines 937–938 fall back to the sidebar context root when row metadata lacks CWD.
The rendered metadata is frozen by `agent-sidebar--plan-view` at line 769.

The probes establish three additional cases beyond I1's pending row:

- A fully parsed native rollout with no CWD launches in the context repository.
- A row shown as `[unreadable transcript]`, due to an invalid thread ID, still launches there.
- After the rollout's CWD changes to another existing directory, `N` uses the old rendered CWD.

A separate control verifies that `RET` and `A` reject the missing-CWD rollout.
This isolates the failure to new-session context selection rather than the parser fixture.
The changed-CWD fixture models a file replacement or correction before the next refresh;
it does not claim that Codex normally rewrites its original CWD.

The impact is a new agent session starting in a repository other than the selected
rollout's repository. Resolve the current row and revalidate its metadata first.
Reject an unreadable or missing-CWD native row instead of using an unrelated context root.

Failing tests:

- `review-workflow-new-rejects-native-missing-cwd`
- `review-workflow-new-rejects-native-error-row`
- `review-workflow-new-revalidates-changed-native-directory`

Passing control: `review-workflow-ret-and-a-reject-native-missing-cwd-control`.

## CW2 — P2: Removing package grouping changes the new-session backend

At line 935, `N` obtains its provider only from a row or a `package` breadcrumb,
then defaults to `agent-shell`. A group can represent only Codex CLI sessions
without containing a package breadcrumb: `(repo model)` is a supported layout.

The fixture creates one Codex CLI rollout, completes metadata parsing, changes
the grouping to `(repo model)`, selects the repository header, and invokes `N`.
It records a generic agent-shell start with configuration name `nil`.
The same repository under default package grouping selects the Codex CLI branch.
Thus a display preference changes which backend receives a new-session action.

Infer the provider when all entries represented by the group have one provider.
For a group with multiple providers, request a choice or require a more specific
selection. Do not silently infer an unrelated provider. Group context should also
retain the Codex home attached to the selected entries.

Failing test: `review-workflow-repo-only-group-keeps-unique-codex-provider`.

## Replay

Run from `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-sidebar-codex/review/run-review.py \
  --probe agent/agent-sidebar-codex/review/round-1/contrarian-workflow-tests.el \
  --selector '"^review-workflow-"' \
  --log agent/agent-sidebar-codex/review/round-1/contrarian-workflow-replay.log
```

The original output is `contrarian-workflow.log`. The baseline exit status is 1,
with four failures and one pass. Preserve that log when replaying against a fix.
