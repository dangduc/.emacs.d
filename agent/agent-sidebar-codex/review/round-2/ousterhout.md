# Round 2 — Ousterhout-inspired cross-review

Reviewed the immutable `round-2-agent-sidebar.el` and `round-2-agent-shell-sidebar.el` sources. The focus was the boundary between display membership, current metadata, and command context. This perspective is inspired by John Ousterhout; it does not claim his participation.

## O4 — P2: Reparsing a group member can launch a session outside the selected group

`agent-sidebar--new-context-for-group` selects members using cached metadata at frontend lines 1146–1152. It then calls `agent-sidebar--new-context-for-entry`, which reparses changed files, at line 1156. The command does not check group membership again after that reparse.

Reproduction:

1. Discover a native Codex rollout whose CWD is repository A, drain parsing, and group by repository.
2. Put point on the repository A group header.
3. Replace the rollout metadata with a different existing CWD, repository B, without refreshing the sidebar.
4. Press `N` through its noninteractive command function.

The displayed header still identifies A. The actual launch recorder receives B. A row action may intentionally use newly validated metadata; a group action must also validate that the entry still belongs to the selected group. Otherwise the action silently changes the selected repository.

`ousterhout-r2-new-on-stale-repo-group-does-not-launch-another-repo` asserts that this stale group produces a user error with zero launches. It fails on the Round 2 snapshot, with B recorded as the launch directory. This probe uses the real metadata reader and context resolution and replaces only the terminal launcher.

Suggested correction: evaluate membership and action context from one validated metadata state. If reparsing invalidates the selected breadcrumb or filter membership, refuse the stale group and request a current row or refresh. Preserve normal row activation's ability to use changed metadata.

## Passing controls and mutation

`ousterhout-r2-filtered-group-resolves-visible-provider` creates Claude and Codex entries under the same repository, filters the view to the Codex prompt, and verifies that group `N` starts Codex. This passes.

`ousterhout-r2-filter-membership-mutation-is-detected` temporarily replaces the membership filter predicate with a constant true result. The same fixture then produces an ambiguity error. The test detects that result. This establishes that the passing control exercises the filter-dependent membership boundary.

## Replay

From `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-sidebar-codex/review/run-review.py \
  --front agent/agent-sidebar-codex/review/round-2-agent-sidebar.el \
  --core agent/agent-sidebar-codex/review/round-2-agent-shell-sidebar.el \
  --probe agent/agent-sidebar-codex/review/round-2/ousterhout-tests.el \
  --log agent/agent-sidebar-codex/review/round-2/ousterhout-replay.log \
  --selector '"^ousterhout-r2-"'
```

Result: two controls pass; one desired-invariant assertion fails. Full output is retained in `ousterhout.log`. No production changes were made in this round. No model or external session was started.
