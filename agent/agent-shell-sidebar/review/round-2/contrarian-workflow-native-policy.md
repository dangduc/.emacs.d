Final integration exposed a test harness defect when native subroutine trampolines were enabled. This addendum corrects the two affected deletion assertions. No product source changed.

The cancellation test replaced `delete-file` with an unconditional failure. Installing its `yes-or-no-p` stub could trigger native compilation, whose scratch-file cleanup also calls `delete-file`. The combined-suite backtrace identified that cleanup call. The coordinator preserved the initial failures in `review/before-workflow-isolation-source-tests.log` and `review/before-workflow-isolation-emacs30-tests.log`.

The cancellation stub now fails only for this fixture's transcript paths. Unrelated cleanup uses the captured real `delete-file`. Both cancellation forms, mark counts, file-existence checks, and subsequent metadata completion assertions remain.

A fresh isolated run exposed the same counting problem in the partial-deletion test: it observed three calls instead of the two transcript attempts. That failure remains in [contrarian-workflow-native-partial-before.log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow-native-partial-before.log). The counter now includes only fixture paths. An additional assertion requires those paths to equal the complete fixture file list. The real successful deletion, injected file error, and retained visible mark checks remain.

The final [test file](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow.el) and [frozen copy](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow-native-fixed.el) have SHA256 `427f3b5ed0e4eae843dd08c04cbd75811f0a2bbc067ae0604de8b33e83b0c811`.
The tested product source is `review/integration/lisp/agent-shell-sidebar.el`, SHA256 `4c2e73058c39fe01215d879ffc882005cb980f06f05d4a4e0f72c0461a7546b3`.

The [control loader](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow-native-policy.el) preserves the incoming compilation setting while loading the reviewer file. It verifies that `native-comp-enable-subr-trampolines` is `t` before and after ERT.

| Control | Observed result |
| --- | --- |
| All eight workflow tests with native policy `t` | 8/8 pass; policy remains `t` |
| Partial-deletion test alone in a fresh process with policy `t` | 1/1 passes; both fixture paths were attempted |
| Replace execution with an attempted deletion of a marked fixture transcript | The cancellation test fails with `Deletion occurred after cancellation`; expected exit status 1 |

The negative control confirms that allowing unrelated cleanup did not disable the fixture deletion guard. It never deletes the transcript because the guard fails first.
Logs: [eight tests](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow-native-policy.log), [isolated partial deletion](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow-native-partial.log), and [negative control](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow-native-negative.log).

Run from `/Users/ducnguyen/.emacs.d`:

```sh
/usr/bin/python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-2/contrarian-workflow-native-policy.el --source agent/agent-shell-sidebar/review/integration/lisp/agent-shell-sidebar.el --log agent/agent-shell-sidebar/review/round-2/contrarian-workflow-native-policy.log
CW2_NATIVE_CONTROL=partial-delete /usr/bin/python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-2/contrarian-workflow-native-policy.el --source agent/agent-shell-sidebar/review/integration/lisp/agent-shell-sidebar.el --log agent/agent-shell-sidebar/review/round-2/contrarian-workflow-native-partial.log
CW2_NATIVE_CONTROL=fixture-delete /usr/bin/python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-2/contrarian-workflow-native-policy.el --source agent/agent-shell-sidebar/review/integration/lisp/agent-shell-sidebar.el --log agent/agent-shell-sidebar/review/round-2/contrarian-workflow-native-negative.log
```

These targeted runs used Emacs 31.1 with installed dependencies, synthetic local files, and fresh batch processes. The coordinator owns the subsequent full 131-test source, compiled, and Emacs 30 runs. This addendum does not claim those results.
