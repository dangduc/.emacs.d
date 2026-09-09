The isolated [source](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/fixes/metadata.el) fixes OUSTERHOUT-2, T2, CE-1, CE-2, and the FIFO discovery/read failure.
The [patch](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/fixes/metadata.patch) uses the requested `a/lisp/agent-shell-sidebar.el` and `b/lisp/agent-shell-sidebar.el` labels.
Its base is `review/baseline.el`, SHA256 `ef3848084626c7ede22c2947bfdb16b1901e6201bab735bc96caba84356973a7`.

Each sidebar owns its cache and discovery snapshot. A refresh in one buffer cannot replace another buffer's parsed metadata.
Redraw still uses cached metadata without disk access. Failed metadata reads remain available for display, but refresh and activation retry them.
Each refresh queues each failed path once. The existing chunk scheduler bounds this work.

The parser still reads at most 8192 bytes. Only complete metadata lines count when the file extends beyond that limit.
A complete field at true EOF remains valid without a final newline.
An incomplete metadata section produces a clear error that stops activation, including fresh session creation.
The parser retains complete fields for display. A complete header followed by a long message remains valid.
Both the header separator and first message remain metadata boundaries.

The CWD parser removes one formatting separator after the label. It preserves further spaces and tabs exactly.
Real writer fixtures cover paths with a final space, final tab, initial space, and a directory named with one space.
An empty CWD remains distinct from a CWD that contains a space.

Discovery checks the mode field as well as the file type. The parser repeats this check before the synchronous read.
The parser also checks attributes after the read to detect file growth across the byte limit.
FIFO fixtures cover initial discovery, replacement after discovery, and replacement after activation reads its first attributes.

The final runs used Emacs 31.1 and agent-shell 0.74.3:

| Run | Result | Log |
| --- | --- | --- |
| New filesystem regressions | 12/12 passed | [metadata-tests.log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/fixes/metadata-tests.log) |
| Original metadata probes and controls | 14/14 passed | [metadata-original-probes.log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/fixes/metadata-original-probes.log) |
| Original sidebar suite | 33/33 passed | [metadata-baseline-suite.log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/fixes/metadata-baseline-suite.log) |
| Original FIFO helper | Exit 0, no timeout, 0.198 seconds | [metadata-fifo.log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/fixes/metadata-fifo.log) |

The original probe wrapper loads the reviewer files without edits. It excludes five tests assigned to other fix owners.
These tests cover fallback identity, substring identity, deletion through aliases, disabled refresh, and ordinary-window toggle behavior.
Every retained assertion runs unchanged, including both metadata-boundary controls and the real writer probe.
The FIFO assertion also passes through its original ERT wrapper, with a 0.183-second helper run.

Run these commands from `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/fixes/metadata-tests.el --source agent/agent-shell-sidebar/review/fixes/metadata.el --ert --log agent/agent-shell-sidebar/review/fixes/metadata-tests.log
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/fixes/metadata-original-probes.el --source agent/agent-shell-sidebar/review/fixes/metadata.el --ert --log agent/agent-shell-sidebar/review/fixes/metadata-original-probes.log
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-1/contrarian-evidence-suite.el --source agent/agent-shell-sidebar/review/fixes/metadata.el --ert --log agent/agent-shell-sidebar/review/fixes/metadata-baseline-suite.log
python3 agent/agent-shell-sidebar/review/round-1/contrarian-workflow-fifo.py agent/agent-shell-sidebar/review/fixes/metadata.el > agent/agent-shell-sidebar/review/fixes/metadata-fifo.log
```

The fixtures use temporary local files, buffers, and the existing local mock ACP process. No external agent service or live Emacs instance participated.
The permission fixture requires a user that cannot read a mode-000 file. Its readability assertions passed in this environment.
Remote file handlers remain outside this test scope.

The type check and `insert-file-contents` are separate operations. A replacement in the final stat/open interval remains a possible race.
The changes cover the reported replacement after discovery and activation attribute lookup. They do not claim an atomic regular-file open.

Integration must preserve the cache initialization in `agent-shell-sidebar-mode`.
The scheduling owner can retain the cache entry format `(:schema ... :signature ... :header (:error ...))` for worker failures.
Only `review/fixes/metadata*` files changed for this subtask.
