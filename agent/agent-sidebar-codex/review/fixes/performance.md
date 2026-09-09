# Fix for LUU-R1-1: bounded previews

The shared provider dispatcher now limits each cached preview to 200 characters. A truncated preview contains the first 199 characters and an ellipsis. Short strings, empty strings, and absent previews retain their previous values. The limit applies to Codex CLI, Claude CLI, agent-shell transcripts, and custom providers in `agent-sidebar-mode`.

Sidebar filtering searches this bounded preview, along with the existing path, repository, agent, model, session ID, and working-directory fields. Text beyond the preview limit no longer matches a sidebar preview search. The `o` command still opens the complete transcript in View mode. The change does not rewrite transcript files or replace session metadata used for activation.

Implementation: `agent-sidebar--bounded-preview` is applied in `agent-sidebar--parse-file` before the metadata enters the cache. Providers retain ownership of their original result objects. The parent task handles invalidating cached metadata in existing Emacs buffers during reload.

## Evidence

[Tests](./performance-tests.el) cover the shared provider contract, all three built-in providers, empty and short values, the 200-character boundary, Unicode round trips, and unchanged provider metadata.

The 300-session fixture uses the same 200,000-character prompt lines as the Round 1 reproduction. All 300 rows retain their session IDs, models, and working directories. The sidebar contains **67,270 characters**, compared with **60,007,270** before the fix. The measured synchronous filter took **17.9 ms** in this run, compared with **601 ms** in the recorded baseline run. Timing is diagnostic; there is no timing-based test assertion.

A warm refresh performs zero additional parser calls. The test opens one complete 200,000-character prompt through the actual transcript command, verifies View mode, and compares its SHA-256 digest with the original file content.

Result: **3/3 tests pass** against the updated source. The same three tests all fail against the immutable original snapshot at their size/length assertions. This confirms that the regression assertions detect the missing bound. The original Round 1 reproduction log is preserved.

## Replay

```sh
python3 /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/run-review.py \
  --front /Users/ducnguyen/.emacs.d/lisp/agent-sidebar.el \
  --core /Users/ducnguyen/.emacs.d/lisp/agent-shell-sidebar.el \
  --probe /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/fixes/performance-tests.el \
  --selector '"^performance-"' \
  --log /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/fixes/performance.log
```

Omit `--front` and `--core` to reproduce the expected failures against the original snapshot. Save that output separately as `performance-original-oracle.log`.

The suite loads Round 1 benchmark helpers. Select `"^performance-"` to run these fixed-behavior regressions; the Round 1 oversized-buffer assertions intentionally describe the original problem.
