# Round 2: Dan Luu-inspired performance review

This review uses a measurement-focused analytical perspective inspired by Dan Luu. It is not a review by Dan Luu.

Inputs are the immutable `round-2-agent-sidebar.el` and `round-2-agent-shell-sidebar.el` snapshots. The two new probes create synthetic Codex files and call the group-context resolver. They do not launch terminals, start agent services, or read private transcripts.

## Finding LUU-R2-1: P2 — Stop group validation when ambiguity is established

Frontend lines **1155–1158** parse and validate every matching entry before checking whether the group contains multiple session contexts. `agent-sidebar-new-session` calls this resolver synchronously when `N` is used on a group header.

The cold-sidebar fixture contains 300 valid rollouts in one Codex package group. Their recorded directories alternate between two existing directories, so the first two metadata reads establish that the group is ambiguous. The files are stable before discovery; this case does not involve the separately reported changed-directory race.

Each file contains 20,000 characters of session instructions and enough complete event records to fill the bounded read prefix. The existing resolver nevertheless calls the parser **300 times** and takes **2.297 seconds** before reporting that a specific row must be selected. This is synchronous command work; the resolver has no equivalent of the idle parser's between-file input check.

The positive control performs the ambiguity check as soon as a second distinct context appears. With the same fixture, it calls the parser **twice** and returns the same user-error category in **14 ms**. The control changes no selection semantics: additional entries cannot make two already distinct contexts become one.

Suggested fix: reject the group immediately after inserting a second distinct context into `contexts`. Keep the existing context revalidation and group-membership safety checks. Add a regression that makes parsing a third member fail once the first two have established ambiguity. This can accompany the workflow fix in the same function.

The dense-event fixture measures a stress case within the existing prefix bound, not the average cost of every Codex history. The unnecessary 298 parser calls are deterministic regardless of machine speed. Timing is recorded evidence, not a pass/fail threshold.

## Passing cache control

A second fixture contains 300 sessions with one shared directory and already parsed metadata. The group resolver returns the expected Codex provider, directory, and home in **9 ms**, with **zero parser calls and zero discovery calls**. This establishes that the ordinary warm path reuses metadata and does not rescan the store.

## Replay

```sh
python3 /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/run-review.py \
  --front /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-2-agent-sidebar.el \
  --core /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-2-agent-shell-sidebar.el \
  --probe /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-2/luu-tests.el \
  --selector '"^luu-r2-"' \
  --log /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-2/luu.log
```

Result: **2/2 probes passed**, taking 5.34 seconds on Emacs 31.1. The first test asserts the excessive parser count to reproduce the frozen snapshot; its baseline assertion should fail after the fix. The guarded control and warm-cache checks describe the desired behavior. No product files were edited in this review.
