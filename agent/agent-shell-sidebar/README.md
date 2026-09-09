# Agent-shell sidebar

The later [provider merge](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/README.md) adds Claude CLI sessions and configurable grouping.
The menu now opens `agent-sidebar-toggle-sidebar`.
This document records the original implementation and its two review rounds; the merge notes describe the current combined files and checks.

The completed sidebar is installed at [lisp/agent-shell-sidebar.el](/Users/ducnguyen/.emacs.d/lisp/agent-shell-sidebar.el).
The [Desktop copy](/Users/ducnguyen/Desktop/agent-shell-sidebar.el) contains the same source.
The [original file](original.el) is preserved for comparison.

Open the sidebar with `M-x agent-shell-sidebar-toggle-sidebar`.
The existing agent-shell menu also has a `b` entry.
The configuration registers these commands for later Emacs starts.
The commands are also available in the current Emacs process.

| Key | Action |
| --- | --- |
| `RET` | Resume a transcript session, reuse its existing shell, or fold a header |
| `o` | Read the transcript in View mode |
| `TAB` | Fold or unfold a project or agent section |
| `/` | Filter transcript paths, prompts, agents, models, directories, and session IDs |
| `C-u /` | Clear the filter |
| `N` | Start a new session in the project at point |
| `g` | Refresh the file list |
| `n` / `p` | Move between rows |
| `d` / `u` / `U` | Mark, unmark, or clear all deletion marks |
| `x` | Confirm deletion of marked transcripts |
| `q` | Hide the sidebar |

Evil normal state also supports `j`, `k`, `gr`, `gg`, `G`, `ZZ`, and `ZQ`.
Filtering uses literal text without case sensitivity.
Pending rows remain visible until the parser reads their metadata.

## Completed behavior

Discovery combines project.el, Projectile, extra roots, open agent-shell directories, and the directory from which the sidebar opens.
Each refresh scans project directories once.
Sorting and redraws use the saved file attributes.
The sidebar skips inaccessible projects, remote projects, and transcript paths that are not regular files.
This excludes directories, symbolic links, and named pipes.

Each sidebar owns its metadata cache, file snapshot, timers, and queues.
Each timer parses at most 30 files by default.
Successive chunks run during continuous idle time and yield when input waits.
Staged views spread path processing and row construction across callbacks.
Completed snapshots show parsed metadata while other files remain queued.
Publication still copies the full view and restores each window position.
Completed commands reset idle thresholds for every sidebar.
Closing the buffer or changing its mode cancels its timers and discards staged content.
A read-hook exception retains the file for one retry.
A repeated exception produces an error row and lets other files proceed.
A render error stops publication until a manual refresh.
Hidden sidebars skip automatic directory refreshes.
Redraws preserve the selected row, scroll position, folds, and deletion marks.

The parser reads at most 8192 bytes per transcript.
It accepts complete metadata lines and refuses a header cut by this limit.
Working-directory values preserve significant whitespace.
Read errors are retried on refresh, including after file permissions change.
Metadata ends at the header separator or first message heading.
Text in a prompt cannot supply the session ID, working directory, model, or agent name.
The cache signature includes modification time, size, inode, and parser schema.
Activating a row checks the current file attributes again.

Resumption uses the recorded working directory, including a project subdirectory.
Without a recorded directory, the sidebar uses the transcript's project root.
A recorded directory that no longer exists produces an error.
An unknown or ambiguous agent name opens the agent selector.
An empty configuration name never matches another agent.
Existing and connecting shells match by session ID, resolved agent configuration, and directory.
Once an active session ID exists, it replaces the pending resume ID for reuse checks.

Transcript display uses an ordinary window and preserves dedicated sidebars.
The transcript opens in View mode.
Deletion honors `delete-by-moving-to-trash`, retains failed marks, and refuses transcripts owned by open agent-shell buffers.
The ownership check includes directory aliases and hard links.
Toggle and hide recognize side windows while preserving ordinary views of the same buffer.

## Configuration

The implementation requires agent-shell 0.74.3 or later.
It uses agent-shell's internal startup and state APIs.
Future changes to those APIs can require sidebar updates.

Additional standard transcript roots use this setting:

```elisp
(setq agent-shell-sidebar-extra-project-roots
      '("~/dev/project-a/" "~/dev/project-b/"))
```

The standard layout is `<root>/.agent-shell/transcripts/*.md`.
Custom transcript paths outside this layout are not discovered automatically.
Metadata beyond the first 8192 bytes is unavailable to the sidebar.

Remote discovery is disabled by default because remote file operations can wait for a connection.
The setting `agent-shell-sidebar-include-remote-projects` enables it.
The setting `agent-shell-sidebar-refresh-timer` accepts an idle interval or `nil` to disable automatic refresh.

## Evidence

Two review rounds used all six requested perspectives and executable probes.
Delegates fixed 15 product defects and corrected test-harness isolation problems.
The [review index](review/README.md) links every finding, fix, replay command, and limitation.
Both the initial implementation and the reviewed source snapshots remain available.

The combined ERT runner has 131 tests.
All 131 pass with source and compiled sidebar code on Emacs 31.1.
All 131 also pass on Emacs 30.2.50 with dependency sources copied into a temporary directory.
Compilation treats warnings as errors and passes.
The logs cover [source](review/final-source-tests.log), [bytecode](review/final-compiled-tests.log), and [Emacs 30](review/final-emacs30-tests.log).

The [ACP fixture](tests/acp-fixture.py) uses the installed agent-shell startup, handshake, session creation, and resumption code.
It checks the recorded subdirectory and reuses the existing shell on repeated activation.
It runs as a local Python process without contacting an external provider.
The final suite also includes the unchanged probe that exposed refresh during a read hook.

The [Round 2 mutation checks](review/round-2/contrarian-evidence.md) detect four deliberate parser regressions.
These remove the read bound, remove the first-message boundary, accept partial fields, or trim significant directory whitespace.
Both assertion gaps from the original 33-test suite are covered by independent controls.
The original-file regression results remain in the [earlier log](original-tests.log).

The 3,000-transcript benchmark reads 3,000 headers and performs one discovery scan.
It measures every callback until parsing and staged rendering finish, including snapshot publication.

| Measurement | Review baseline | Final code |
| --- | ---: | ---: |
| Transcript rows constructed | 303,000 | 21,000 |
| Total measured work, including initial refresh | 22.445 s | 1.842 s |
| Maximum callback | 282.6 ms | 45.4 ms |
| Initial synchronous refresh | 237.0 ms | 227.8 ms |

These are individual local batch runs with a warm operating-system cache.
The final run had no attached sidebar window.
The measurements exclude idle delays and screen redisplay; they are not end-to-end completion times.
Four snapshots published while parsing remained pending.
The [baseline log](review/fixes/scheduling-baseline-scale.log) and [final log](review/final-scale.log) record the method and counters.
The [Luu review](review/round-2/luu.md) separately measures attached-window and long-preview cases.

The running Emacs loaded the final bytecode from the installed library path.
A real idle-timer check parsed and rendered all 71 temporary transcripts in about 0.88 seconds.
It made no manual callback calls.
The [live result](live-check.eldata) records the library, compiled guard, menu binding, counts, and process ID.
The [cleanup check](review/final-live-cleanup.eldata) confirms no fixture buffer, staged buffer, or sidebar timer registration remained.

## Remaining limits

Initial refresh and explicit redraw commands still render the full view synchronously.
In the long-preview fixture, a 21 MB view took 651–683 ms for a warm manual refresh.
Publication and row-position searches also scale with view size and visible window count.
The path-operation bound does not bound file-read latency, garbage collection, or callback duration.

Regular-file checks precede reading and opening, but those operations are not atomic.
A concurrent replacement in the remaining check/open interval is outside the tested guarantee.
Provider authentication and remote session availability were not tested.

The unfinished-prefix-key idle case remains unverified.
The isolated terminal editor failed to initialize, so its planned input history did not run.
The [Kingsbury controls and limitation](review/round-2/kingsbury.md) retain the exact startup evidence.
Native screen automation also failed to start, so there was no visual screenshot check.

## Repeat the checks

Run these commands from `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-shell-sidebar/run-tests.py --compile
python3 agent/agent-shell-sidebar/review/run-regressions.py
python3 agent/agent-shell-sidebar/review/run-regressions.py --source lisp/agent-shell-sidebar.elc
python3 agent/agent-shell-sidebar/review/run-regressions.py --emacs /Applications/Emacs-30.app/Contents/MacOS/Emacs --source-dependencies
python3 agent/agent-shell-sidebar/review/fixes/scheduling-scale.py --source lisp/agent-shell-sidebar.el --sizes 3000 --log agent/agent-shell-sidebar/review/final-scale.log
```

Fixtures use temporary directories and do not delete user transcripts or contact external agent services.
