Round 1 reviewed the frozen source through a Kingsbury-inspired lens: event schedules, cancellation, and failure recovery.
This report does not represent statements by Kyle Kingsbury.
The source SHA256 is `ef3848084626c7ede22c2947bfdb16b1901e6201bab735bc96caba84356973a7`.

**[P2] A transient read-hook error stops the parse queue.**

Location: [baseline.el:358](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:358), through line 361.
The corresponding live source has the same line numbers and SHA256.
The callback removes the file before the read succeeds.
An error also skips the call that schedules the next chunk.

The probe used two readable temporary transcripts and disabled automatic refresh with the supported `nil` setting.
It delivered one error through Emacs's standard `after-insert-file-functions` hook.
The real `timer-event-handler` ran the sidebar callback and the real `insert-file-contents` ran the hook.
The hook existed only during that callback.

Expected: after the transient error, each unparsed file remains pending or has a reported read failure, and pending work has a timer.
Actual: `one.md` had no cache entry and no queue entry.
`two.md` remained queued, but `agent-shell-sidebar--parse-timer` was `nil`.
The dispatcher reported the error and returned.
No parser callback remained to complete either transcript.
Manual refresh can restart the work after the hook error disappears.

The probe injected the hook error.
It demonstrates recovery behavior at an Emacs extension boundary, without evidence of a particular installed hook that causes this error.
The probe did not simulate keyboard input or contact an external agent.

Fix direction: retain each queue entry until the read returns a result.
Use cleanup protection to schedule remaining work after a callback error.
Apply a bounded retry or an explicit failure state to the affected file.
This limit prevents an endless retry loop after a persistent hook error.

Three independent controls passed:

- A cancelled timer did not run its callback through the real dispatcher.
- A major-mode change cancelled the scheduled timer before dispatch.
- A content change between file attributes and the read produced an old cache signature, but the next activation repaired it.

The read-race control found no persistent wrong-session result in that schedule.
Idle-age reset, cached permission failures, and deletion through project aliases belong to other round-1 reports.

Evidence: [probe](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/kingsbury-probe.el), [log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/kingsbury-probe.log).
The run completed with three passing controls and one failing recovery assertion.
Exit status was `1`.

```sh
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-1/kingsbury-probe.el --source agent/agent-shell-sidebar/review/baseline.el --ert --log agent/agent-shell-sidebar/review/round-1/kingsbury-probe.log
```
