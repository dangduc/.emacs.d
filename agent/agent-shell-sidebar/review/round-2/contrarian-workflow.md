Round 2 found no additional workflow defect beyond **R2-T1**, which the Torvalds review had already reported. This review independently reproduces that defect and verifies the isolated viewing fix.

The frozen source is [reviewed.el](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/reviewed.el), SHA256 `629a0b61dc25a1b9766936b34b655aea9e1775c57f0f2d98ebb9ef00fea1b6ed`.
The viewing candidate is [viewing.el](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/fixes/viewing.el), SHA256 `dc4ebbc29ae95c93c5bf705724488a24e053f57a68a90a47db89740c4c1b18f1`.

**R2-T1, P2: changed transcript paths can block the View command.**

The call at [reviewed.el:1144](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/reviewed.el:1144) opens any readable path with `find-file-noselect`. The preceding readability check accepts a FIFO.
The fresh helper first creates a regular `.md` transcript and verifies that normal discovery includes it. It then replaces the file before invoking the real View command.
The frozen source blocks on the FIFO and does not return before the three-second deadline. The helper kills only its own batch subprocess.
The viewing candidate checks regular-file attributes at [viewing.el:1144](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/fixes/viewing.el:1144) and reports a user error in 0.216 seconds.

| Path after discovery | Frozen source | Viewing candidate |
| --- | --- | --- |
| Original regular file | Opens original contents in View mode | Same |
| Replaced regular file | Opens replacement contents in View mode | Same |
| Removed file | Reports unreadable transcript | Same |
| Directory | Opens a Dired buffer and enables View mode | Rejects the nonregular path |
| Symbolic link | Opens the target contents | Rejects the nonregular path |
| FIFO with no writer | Times out after 3.005 seconds | Rejects the nonregular path in 0.216 seconds |

The directory and symbolic-link controls establish that the guard follows the discovery policy. They do not create separate findings.
Both regular-file controls use the real file-reading and View-mode code. The helper replaces only the final display operation so it can inspect the opened buffer.
The separate window test below exercises actual display and selection.

The [helper](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow-view.py), [frozen output](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow-view-frozen.json), and [candidate output](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow-view-fixed.json) preserve commands, source fingerprints, observed contents, errors, and timings.
The [assertion script](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow-view-assert.py) checks the independent results. Its [log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow-view-assert.log) records a pass.

The new [workflow probe](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow.el) contains eight tests. All eight pass on both the frozen source and the viewing candidate.
These tests exercise public commands and real local buffers, windows, file reads, discovery, redraws, and timer dispatch.

| Fresh workflow | Expected and observed |
| --- | --- |
| Mark a transcript while a 420-file metadata snapshot is partially constructed | The command cancels the obsolete staging buffer. Metadata finishes in 24 further timer callbacks. The visible `D` mark remains, then the unmark command removes it. |
| Decline or quit deletion while metadata is pending | Both cancellation forms retain the two marks and every file. Subsequent metadata completion preserves both visible marks. |
| Delete two marked transcripts when one deletion fails | One real temporary file is deleted. An injected file-error leaves the other file and its visible `D` mark. Refresh and metadata completion do not lose that mark. |
| Cancel selection between two live sessions with the same recorded agent and session ID | The selector runs once. Neither startup nor display occurs, and point remains on the transcript. |
| Explicitly select the second configuration in that ambiguous case | The second live buffer is reused. The first buffer is not selected, and no agent starts. |
| Open a regular transcript from a dedicated side window | The ordinary window becomes selected and displays the file in View mode. The side window keeps the sidebar buffer and its dedication. |
| Hide a side window while two ordinary windows also display the sidebar | Only the side window closes. Both ordinary views remain. |
| Disable refresh after scheduling, then invoke manual refresh | The pending callback performs no scan. The explicit refresh performs exactly one scan and finishes its metadata work. |

The ambiguous-session tests use synthetic agent-state buffers and replace the selector, startup boundary, and final display call. They still run the actual configuration resolution and session-reuse code.
The deletion test injects a file error for one known temporary path; deletion of the other path uses the real `delete-file` implementation.

The [frozen workflow log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow.log) and [candidate workflow log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow-viewing.log) record 8/8 passes and source provenance before and after the tests.
The probe SHA256 is `27f6358a6195ce63607fbafb057f8f6c79a99652a391a1365950e9b1cd5eb6ce`.

Round 1 dispositions:

- **CW1:** the original bounded parser helper now excludes the FIFO from discovery and returns an explicit nonregular-file error. It exits in 0.213 seconds. The [new control output](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow-parser-fifo.json) records that result. R2-T1 exposed a separate synchronous read path; the viewing candidate closes that path for the tested replacement.
- **CW2:** the new two-ordinary-view history verifies that sidebar lookup and hide operations distinguish actual side windows. Regular-file opening also preserves the dedicated side window.
- **CW3:** the fresh disabled-callback history passes and confirms that explicit refresh remains available.
- The identity, metadata, and scheduling fixes compose in the new cancellation, selection, mark-preservation, and partial-deletion histories. This review does not claim to retest every other reviewer's finding.

Run these commands from `/Users/ducnguyen/.emacs.d`:

```sh
/usr/bin/python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-2/contrarian-workflow.el --ert --source agent/agent-shell-sidebar/review/round-2/reviewed.el --log agent/agent-shell-sidebar/review/round-2/contrarian-workflow.log
/usr/bin/python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-2/contrarian-workflow.el --ert --source agent/agent-shell-sidebar/review/fixes/viewing.el --log agent/agent-shell-sidebar/review/round-2/contrarian-workflow-viewing.log
/usr/bin/python3 agent/agent-shell-sidebar/review/round-2/contrarian-workflow-view.py agent/agent-shell-sidebar/review/round-2/reviewed.el > agent/agent-shell-sidebar/review/round-2/contrarian-workflow-view-frozen.json
/usr/bin/python3 agent/agent-shell-sidebar/review/round-2/contrarian-workflow-view.py agent/agent-shell-sidebar/review/fixes/viewing.el > agent/agent-shell-sidebar/review/round-2/contrarian-workflow-view-fixed.json
/usr/bin/python3 agent/agent-shell-sidebar/review/round-1/contrarian-workflow-fifo.py agent/agent-shell-sidebar/review/round-2/reviewed.el > agent/agent-shell-sidebar/review/round-2/contrarian-workflow-parser-fifo.json
/usr/bin/python3 agent/agent-shell-sidebar/review/round-2/contrarian-workflow-view-assert.py > agent/agent-shell-sidebar/review/round-2/contrarian-workflow-view-assert.log
```

The runs used fresh batch Emacs processes with installed agent-shell dependencies and temporary synthetic transcripts. No external agent service or live Emacs process participated.
Only `round-2/contrarian-workflow*` artifacts changed.

Limits: the regular-file check and file open remain separate operations. These checks do not establish atomic protection against replacement in the final stat/open interval.
The tests do not cover remote filesystem handlers, graphical mouse input, or the unresolved idle-timer prefix-command seam. That seam remains unconfirmed and was not retested here.

Final integration required two test-only corrections for native compiler scratch-file cleanup. The [native-policy addendum](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-workflow-native-policy.md) records the preserved failures, scoped assertions, causal control, and final test-file fingerprint.
