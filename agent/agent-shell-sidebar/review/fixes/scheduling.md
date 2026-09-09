The isolated scheduling copy corrects four findings: repeated full redraws, stale idle thresholds, lost work after a read-hook error, and disabled refresh callbacks.
The source is `review/fixes/scheduling.el`.
The patch uses `a/lisp/agent-shell-sidebar.el` and `b/lisp/agent-shell-sidebar.el` labels.

The parse callback now constructs staged snapshots through the built-in Emacs generator.
Each callback reads at most the configured header count and advances at most 300 path or row operations.
Input checks precede each operation.
Metadata changes request another snapshot without discarding the current construction work.
Completed snapshots publish before the full parse queue finishes.

Each snapshot captures a header together with its file attributes.
Grouping, filtering, and row display use that captured metadata.
The snapshot preserves project order, agent order, transcript order, folds, and marks.
Publication records each current window view and restores its row identity, column, and scroll position.
A refresh, filter change, or fold command discards obsolete staged content before a synchronous redraw.

A global command hook restarts timers for registered sidebar buffers after input anywhere in Emacs.
Continuous idle periods still use increasing thresholds.
A new idle period starts with the configured parse delay and refresh interval.
Buffer destruction and major-mode changes remove their timer registration.
A disabled refresh callback skips discovery and does not schedule another refresh.

A failed read retains the queue head for one retry on the next callback.
A second exception stores an explicit error header and advances the queue.
The callback rearms remaining work through `unwind-protect`.
A quit retains pending work and propagates to the caller.
A quit during row construction discards the partial staging buffer.
A renderer error stops further render attempts and displays a refresh instruction.
A manual refresh clears that error.

All 33 existing tests passed without changes to the shared suite during this run.
All 13 new scheduling tests passed.
All four original Kingsbury probes passed, including the standard insertion-hook exception through `timer-event-handler`.
The bytecode build passed with warnings treated as errors.
The injected renderer-error test emits its expected failure message.

The new tests cover these schedules:

- Input in another buffer resets both timers and cancels the prior timer object.
- Disabled refresh performs no scan through the real dispatcher.
- Transient and persistent standard read-hook exceptions preserve progress.
- A parser quit retains the queue head.
- A renderer quit discards a partially written row.
- Input yields without advancing the queue or staging buffer.
- A major-mode change cancels the timer and deletes the staging buffer.
- Refresh, filter, and fold commands invalidate obsolete snapshots.
- An attribute change after snapshot capture cannot invalidate the captured header.
- Intermediate snapshots preserve agreement between agent headers and transcript rows.
- Publication preserves two window views and deletion marks.
- A persistent renderer error stops retries until a manual refresh.

The measurements use temporary transcripts with alternating Alpha and Beta agents.
Both sources use the same fixture and the real timer dispatcher.
Each size runs once with a fresh metadata cache.
The operating-system cache remains warm.
The measurements include every callback through final snapshot publication.
They exclude idle delays and screen redisplay.

| Files | Source | Initial refresh | Median callback | P95 callback | Maximum callback | Total synchronous work | Rows constructed |
| ---: | :--- | ---: | ---: | ---: | ---: | ---: | ---: |
| 300 | Baseline | 23.1 ms | 23.0 ms | 23.3 ms | 23.3 ms | 0.252 s | 3,300 |
| 300 | Fix | 22.2 ms | 2.6 ms | 22.7 ms | 22.7 ms | 0.128 s | 1,500 |
| 1,200 | Baseline | 91.0 ms | 87.1 ms | 102.8 ms | 104.0 ms | 3.693 s | 49,200 |
| 1,200 | Fix | 90.0 ms | 3.7 ms | 23.1 ms | 23.6 ms | 0.707 s | 8,400 |
| 3,000 | Baseline | 237.0 ms | 216.5 ms | 236.3 ms | 282.6 ms | 22.445 s | 303,000 |
| 3,000 | Fix | 228.7 ms | 5.7 ms | 24.6 ms | 42.9 ms | 1.808 s | 21,000 |

The fix used 12, 54, and 126 callbacks at these sizes.
The maximum callback constructed 296, 300, and 300 transcript rows, respectively.
Every run read exactly N headers and performed one discovery scan.
The three runs published three, four, and four intermediate metadata snapshots before parsing ended.
At 3,000 files, the fix published six snapshots in total.

A separate 3,000-file run displayed the sidebar in one batch window.
The initial window position was near the middle of the transcript list.
That run included view restoration in every publication.
It took 1.818 seconds in total, with a 24.4 ms P95 callback and a 42.9 ms maximum callback.
Its maximum publication took 23.8 ms, including any garbage collection during that operation.
The two-window regression separately checks distinct scroll positions and transcript selection.

Publication still copies the full buffer and searches for retained row identities.
That work scales with the snapshot size and visible window count.
The operation limit bounds path processing and row construction, not total callback time.
Garbage collection, synchronous file reads, and publication can exceed a time budget.
Initial refresh and explicit user redraws also retain their synchronous full rendering behavior.
Hidden sidebars retain pending parse work, as before.
These measurements describe Emacs 31.1 in batch mode on this Mac.

The internal completion contract now includes staged rendering after the last header read.
A test that waits only for an empty parse queue can stop before the final display update.
The new `scheduling--drain` helper waits for the owned parse timer and asserts empty queue, iterator, and dirty state.
It also asserts that no render error remains.
The parent prepared the corresponding shared helper update and a final visible-preview assertion.

Integration must retain the metadata owner's buffer-local cache initialization in sidebar mode.
The staging renderer explicitly binds its captured cache and attribute tables in the output buffer.
The metadata owner also changes the predicate in `--queue-uncached` to retry error headers.
That predicate combines with this patch's pending-count and retry-file initialization.
Other parser/cache functions, identity/deletion functions, and ordinary-window recognition remain outside this patch.

The commands ran from `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/tests/sidebar-tests.el --source agent/agent-shell-sidebar/review/fixes/scheduling.el --ert --log agent/agent-shell-sidebar/review/fixes/scheduling-core.log
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/fixes/scheduling-tests.el --source agent/agent-shell-sidebar/review/fixes/scheduling.el --ert --log agent/agent-shell-sidebar/review/fixes/scheduling-tests.log
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-1/kingsbury-probe.el --source agent/agent-shell-sidebar/review/fixes/scheduling.el --ert --log agent/agent-shell-sidebar/review/fixes/scheduling-kingsbury.log
python3 agent/agent-shell-sidebar/review/fixes/scheduling-scale.py
python3 agent/agent-shell-sidebar/review/fixes/scheduling-scale.py --source agent/agent-shell-sidebar/review/baseline.el --log agent/agent-shell-sidebar/review/fixes/scheduling-baseline-scale.log
python3 agent/agent-shell-sidebar/review/fixes/scheduling-scale.py --sizes 3000 --window --log agent/agent-shell-sidebar/review/fixes/scheduling-window-scale.log
```

This run changed only scheduling artifacts under `review/fixes/`.
It did not change the main source, shared tests, Desktop, configuration, or live Emacs.
It used no external services, commits, or child agents.
