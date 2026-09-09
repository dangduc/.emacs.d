Separate Round 2 review through a Torvalds-inspired lens: API contracts, file operations, and practical failure behavior. The same worker performed the Ousterhout review, but these probes examine different contracts. Neither review claims to represent the named person.

Reviewed frozen [source](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/reviewed.el), SHA256 `629a0b61dc25a1b9766936b34b655aea9e1775c57f0f2d98ebb9ef00fea1b6ed`, and installed agent-shell 0.74.3 on Emacs 31.1.

**T2R-1 — P2: The read command still opens a discovered path that became a FIFO.**

Location: [reviewed.el:1143](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/reviewed.el:1143), immediately before `find-file-noselect` at line 1144.

Trigger: discovery records a regular transcript. Before the user presses `o`, that path is replaced with a named pipe that has no writer. The earlier parser fix does not cover this separate viewing command.

Expected: viewing rejects the nonregular entry before opening it. Actual: `file-readable-p` returns true for the FIFO. `find-file-noselect` then blocks. The isolated Emacs process reached `BEFORE-OPEN ... modes=prw-r--r-- readable=t` and did not return before the four-second deadline. The helper terminated that process. A regular-file control used the same discovery and viewing path and returned with View mode enabled in 0.217 seconds.

The [subprocess probe](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/torvalds-file-open.py) uses real filesystem replacement and the real `find-file-noselect`. It replaces only window display, not file opening. The [log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/torvalds-file-open.log) records the regular control and the 4.007-second timeout.

Reproduce from `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-shell-sidebar/review/round-2/torvalds-file-open.py --source agent/agent-shell-sidebar/review/round-2/reviewed.el --log agent/agent-shell-sidebar/review/round-2/torvalds-file-open.log
```

The command exits with status 1 because viewing did not reject the FIFO. Suggested fix: check current file attributes with the existing regular-file predicate before `find-file-noselect`. Retain the existing missing-file, unreadable-file, and View mode behavior. A separate stat followed by open remains non-atomic; this bounded fix does not claim to eliminate replacement between those operations.

**Original findings and additional controls.**

All three original tests pass in the [original-probe log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/torvalds-original.log). T1 now protects an owned transcript through a directory alias, as well as through the identical path. T2 retries a restored readable file even when mtime, size, and inode remain unchanged.

```sh
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-1/torvalds-probe.el --source agent/agent-shell-sidebar/review/round-2/reviewed.el --ert --log agent/agent-shell-sidebar/review/round-2/torvalds-original.log
```

Two fresh [API controls](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/torvalds-probe.el) pass in the [control log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/torvalds-probe.log):

- Real installed startup rejects a missing agent executable, leaves no agent-shell buffer, and restores the caller's directory and CWD function.
- With the document and sidebar windows both dedicated, fallback display creates an ordinary target window and preserves both existing windows. The fixture sets split thresholds to permit a local split.

```sh
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-2/torvalds-probe.el --source agent/agent-shell-sidebar/review/round-2/reviewed.el --ert --log agent/agent-shell-sidebar/review/round-2/torvalds-probe.log
```

All files and windows belong to temporary batch fixtures. No external agent service or running user Emacs was contacted. The tests do not cover graphical frame creation, remote file handlers, or a replacement that occurs between the proposed attribute check and open. No reviewed source or shared tests were changed.
