# Agent sidebar merge

The later [Codex update](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/README.md) adds native Codex rollouts, ghostel resumption, and an agent-shell action.
The evidence below records the preceding Claude/provider merge.

The supplied provider sidebar is merged with the reviewed sidebar core.
Use `M-x agent-sidebar-toggle-sidebar` or the agent-shell menu's `b` key.
The old `agent-shell-sidebar-*` commands remain available.

Keep both files on `load-path`:

| File | Purpose |
| --- | --- |
| [agent-sidebar.el](/Users/ducnguyen/Desktop/agent-sidebar.el) | Providers, Claude CLI sessions, grouping, and terminal dispatch |
| [agent-shell-sidebar.el](/Users/ducnguyen/Desktop/agent-shell-sidebar.el) | Shared parsing queue, metadata cache, rendering scheduler, windows, and deletion checks |

The installed copies are [agent-sidebar.el](/Users/ducnguyen/.emacs.d/lisp/agent-sidebar.el) and [agent-shell-sidebar.el](/Users/ducnguyen/.emacs.d/lisp/agent-shell-sidebar.el).
The [configuration](/Users/ducnguyen/.emacs.d/lisp/package-declarations.el:1476) registers both sets of commands and selects the provider sidebar from the menu.

## Merged behavior

- Discover agent-shell Markdown transcripts and Claude CLI JSONL sessions.
- Group by provider, repository, model, or date. Use `G` to change the order or select a flat view.
- Use `RET` to resume, `o` to read, `/` to filter, and `TAB` to fold groups.
- Use `N` to start an agent-shell or Claude CLI session in the selected repository.
- Resume Claude CLI through ghostel, vterm, term, or a configured terminal function.
- Reuse a live terminal when its provider, session ID, and directory match.
- Recover missing Claude session IDs when exactly one session matches the recorded directory and starts within 60 seconds.
- Use agent-shell's session picker when a transcript has no session ID and recovery is ambiguous or unavailable.
- Retain deletion marks when a live agent-shell or sidebar-launched terminal owns the transcript, including filesystem aliases.

The default grouping is `(package repo model)`.
Shared settings have both `agent-sidebar-*` and `agent-shell-sidebar-*` names.
For example, either spelling of `extra-project-roots` configures additional agent-shell projects.
Provider entries use absolute regular-file paths as IDs. Parsers return metadata plists.

## Merge decisions

The [incoming file](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/incoming.el) remains unchanged for comparison.
The [previous core](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/core.el) and [previous configuration](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/config.el) are also preserved.

The incoming provider and grouping features now use the reviewed core through buffer-local callbacks.
This retains its cache signatures, reentrant queue guards, bounded rendering work, window handling, and cleanup.
Discovery failures preserve the previous snapshot and pending timer.
Replacing a registered parser invalidates metadata produced by the previous parser.

Claude metadata reads stop at 65,536 bytes and ignore incomplete final records at that limit.
The parser skips metadata prompts, local command messages, and tool-result blocks.
It continues after the first user prompt to find the assistant's model.
It treats JSON `false` as false and skips malformed JSON records.

Claude's project directory encoding loses information.
The sidebar matches known roots or uses the directory recorded in JSONL; it does not reconstruct paths by replacing hyphens with slashes.
The [local encoding probe](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/check-claude-encoding.py) checked 50 session files and found no encoding mismatches.
It outputs counts only.

Terminal arguments remain separate for ghostel and term, and each argument is quoted for vterm's shell command.
The vterm path selects an ordinary window before launch and binds the requested directory in that window's buffer.
A failed ghostel launch removes its unused buffer.

## Validation

The 131 existing core tests and 22 new merge tests pass in each configuration below.
Both installed files compile with warnings treated as errors.

| Configuration | Core evidence | Merge evidence |
| --- | --- | --- |
| Emacs 31.1, source | [131 passed](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/core-regressions.log) | [22 passed](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/final-source-tests.log) |
| Emacs 31.1, bytecode | [131 passed](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/core-compiled-tests.log) | [22 passed](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/final-compiled-tests.log) |
| Emacs 30.2.50, source dependencies | [131 passed](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/core-emacs30-tests.log) | [22 passed](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/final-emacs30-tests.log) |

The [merge tests](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/tests.el) cover both providers, grouping, cache invalidation, refresh during parsing, terminal reuse, and live ownership.
A real local term process records its arguments and working directory.
Ghostel and vterm dispatch checks use test doubles for their inspected local APIs.

The [3,000-file mixed-provider measurement](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/scale.log) recorded one discovery pass and 3,000 header reads.
It built 21,000 rows across the initial view and six published snapshots.
Four snapshots published while parsing remained pending.
Initial refresh took 312 ms, total measured work took 1.46 seconds, and the slowest callback took 59 ms.
This single local batch run excludes idle delays and screen redisplay and has no attached sidebar window.

The running Emacs 31.1 loaded both installed bytecode files and the updated menu.
Its [real idle-timer check](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/live-check.eldata) parsed and rendered 36 Markdown and 36 JSONL fixtures, with both model groups present.
The check never calls parse callbacks manually or starts an external agent.
The [final load check](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/live-final.eldata) confirms the installed libraries and cleanup of fixture buffers and timers.

## Limits

Initial refresh, explicit redraw, snapshot publication, and row-position searches still scale with the full view.
The per-callback operation bound does not bound file latency or garbage collection.
Metadata beyond the Markdown 8,192-byte or JSONL 65,536-byte prefix is unavailable.
The session-ID recovery scan runs on activation and can wait when a project has many uncached sessions.

Provider authentication, remote session availability, and native terminal rendering were not exercised.
Terminal ownership tracking covers sessions launched through this sidebar; it cannot identify every external Claude process.
The previous review's filesystem race and input-history limitations still apply.
Its [two review rounds](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/README.md) remain historical evidence for the core before this merge.

## Repeat checks

Run from `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-sidebar-merge/run.py --compile --log agent/agent-sidebar-merge/compile.log
python3 agent/agent-sidebar-merge/run.py
python3 agent/agent-sidebar-merge/run.py --compiled
python3 agent/agent-sidebar-merge/run.py --emacs /Applications/Emacs-30.app/Contents/MacOS/Emacs --source-dependencies
python3 agent/agent-shell-sidebar/review/run-regressions.py
python3 agent/agent-sidebar-merge/run.py --compiled --probe agent/agent-sidebar-merge/scale.el --log agent/agent-sidebar-merge/scale.log
```

The tests create temporary fixtures and do not contact external agent services.
