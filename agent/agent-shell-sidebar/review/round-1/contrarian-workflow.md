Round 1 produced three findings. Eight independent probes ran against the frozen source. Five controls passed, and three contract assertions failed.

The source SHA256 was `ef3848084626c7ede22c2947bfdb16b1901e6201bab735bc96caba84356973a7`.
The probes used isolated batch Emacs processes, synthetic metadata, temporary filesystem entries, and temporary windows.
They did not access the running Emacs, user transcripts, or external services.

1. **P2: Reject FIFOs before the parser opens them.**

   Source: [baseline.el:214](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:214), with the blocking read at [baseline.el:260](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:260).

   Trigger: a local `.agent-shell/transcripts/pending.md` entry is a FIFO without a writer. Ordinary sidebar discovery includes this entry.
   Expected: discovery excludes the FIFO, and activation rejects a file that changes into a FIFO before parsing.
   Actual: the FIFO has `file-attribute-type=nil`, so discovery accepts it. The parser then waits for input from the FIFO.
   The subprocess did not return before the three-second deadline. The helper killed it and deleted the temporary directory.
   The 8192-byte limit does not bound the time that the file open operation takes.

   Evidence: `workflow-nonregular-fifo-never-blocks-header-parsing` failed. The helper printed `type=nil modes="prw-r--r--" regular=nil`, then `timeout=t elapsed=3.006`.
   Suggested change: use the file mode field to distinguish regular files during discovery. Repeat the type check before each synchronous transcript read.

2. **P2: Distinguish side windows from ordinary windows that show the sidebar buffer.**

   Source: [baseline.el:949](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:949), leading to [baseline.el:1009](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:1009).

   Trigger: the user hides the sidebar, switches to its retained buffer with `C-x b`, and runs the toggle command.
   Expected: toggle opens a side window or safely dismisses the ordinary buffer view.
   Actual: the mode-only lookup identifies the sole ordinary window as the sidebar. Toggle attempts to delete that window and raises an error.
   Evidence: `workflow-toggle-works-after-sidebar-buffer-switched-to-normal-window` failed with `Attempt to delete minibuffer or sole ordinary window`.
   The probe recorded `side=nil windows=1`.
   Suggested change: identify an actual side window in the sidebar lookup. Use a buffer dismissal operation for ordinary window views.

3. **P3: Honor disabled refresh before a pending callback scans projects.**

   Source: [baseline.el:378](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:378).

   Trigger: the sidebar schedules automatic refresh, then the user changes `agent-shell-sidebar-refresh-timer` to `nil` before that callback fires.
   Expected: the disabled callback skips discovery.
   Actual: the pending callback performs one further scan. It does not schedule another callback.
   Evidence: `workflow-disable-refresh-before-pending-callback` dispatched the saved timer through `timer-event-handler`. It recorded `callback scans=1, rearmed=nil`.
   Suggested change: check the refresh setting before the callback calls `agent-shell-sidebar-refresh`.

Passing controls covered agent-selector cancellation, deletion cancellation, ordinary-file discovery, disabled MRU selection, and distinct cursor positions in two windows.
The file discovery control also excluded a directory and a symbolic link with `.md` names.

Run the complete probe from `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-1/contrarian-workflow.el --ert --source agent/agent-shell-sidebar/review/baseline.el --log agent/agent-shell-sidebar/review/round-1/contrarian-workflow.log
```

The command exits with status 1 because the three defect assertions fail.
The [probe](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/contrarian-workflow.el), [FIFO helper](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/contrarian-workflow-fifo.py), and [log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/contrarian-workflow.log) contain the complete evidence.
