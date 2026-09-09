The isolated [identity.el](identity.el) copy corrects OUSTERHOUT-1, OUSTERHOUT-3, T1, and CW-2.
The [patch](identity.patch) uses `a/lisp/agent-shell-sidebar.el` and `b/lisp/agent-shell-sidebar.el` as its file labels.
The baseline SHA256 is `ef3848084626c7ede22c2947bfdb16b1901e6201bab735bc96caba84356973a7`.
The revised SHA256 is `10336ae94ef9a75cee302e547534c6dbe0fb344924bd5caa85f5f974463c048c`.

| Finding | Change | Evidence |
| --- | --- | --- |
| OUSTERHOUT-1 | A nonempty active session ID supersedes the pending resume ID. An empty active ID still permits pending-session reuse. | The original probe runs the installed fallback callback. The new regression also covers empty and missing session IDs. |
| OUSTERHOUT-3 | Activation uses the selected configuration for both reuse and startup. Full configuration equality separates agents that share names or identifiers. | Unique aliases reuse connecting and active shells. Explicit selection reuses the selected agent and separates a later selection. Directory and session controls reject unrelated shells. |
| T1 | Deletion compares filesystem identity after the exact-path check. Absent owner paths do not block unrelated deletion. Filesystem errors retain marks. | Directory aliases work in both directions. Hard links remain protected. A distinct file with identical contents is deleted. |
| CW-2 | Window lookup requires an actual side window. Sidebar creation temporarily permits a split of an ordinary window that displays the same buffer. | Toggle creates a side window beside the ordinary view. A second toggle deletes only the side window. Hide leaves the ordinary view intact. |

Exact live-session access remains available after its configuration disappears from the configured list.
This exception requires no configured name match and an unambiguous exact live match in the requested directory and session.
Configured ambiguity still requires explicit selection.
The existing name-only lookup arguments remain available to callers.

The installed Claude configuration maker returns fresh alists with callback fields on each resolution.
The maker regression records `fresh-alists=t equivalent=t visits=2 starts=1`.
This result supports configuration equality for that installed maker, beyond constant-alist fixtures.

| Command group | Result | Log |
| --- | --- | --- |
| New regressions, revised copy | 13/13 passed, exit 0 | [identity-tests.log](identity-tests.log) |
| Existing suite, revised copy | 33/33 passed, exit 0 | [identity-existing-tests.log](identity-existing-tests.log) |
| Relevant original probes and their controls | 10/10 passed, exit 0 | [identity-relevant-reviewers.log](identity-relevant-reviewers.log) |
| Full Ousterhout probe | 2/3 passed, exit 1. Only OUSTERHOUT-2 failed. | [identity-ousterhout.log](identity-ousterhout.log) |
| Full Torvalds probe | 2/3 passed, exit 1. Only T2 failed. | [identity-torvalds.log](identity-torvalds.log) |
| Full workflow probe | 6/8 passed, exit 1. Only CW-1 and CW-3 failed. | [identity-workflow.log](identity-workflow.log) |
| New regressions, frozen baseline | 2 controls passed. Eleven behavior assertions failed, exit 1. | [identity-tests-baseline.log](identity-tests-baseline.log) |

OUSTERHOUT-2, T2, CW-1, and CW-3 belong to other fix owners.
The selected reviewer run excludes those four tests and retains the four relevant findings and six controls.
The full probe logs preserve the remaining failures without changing their expected results.

The commands ran from `/Users/ducnguyen/.emacs.d` on Emacs 31.1 with installed agent-shell 0.74.3.
These are the commands for the regression suites:

```sh
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/fixes/identity-tests.el --ert --source agent/agent-shell-sidebar/review/fixes/identity.el --log agent/agent-shell-sidebar/review/fixes/identity-tests.log
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/tests/sidebar-tests.el --ert --source agent/agent-shell-sidebar/review/fixes/identity.el --log agent/agent-shell-sidebar/review/fixes/identity-existing-tests.log
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/fixes/identity-tests.el --ert --source agent/agent-shell-sidebar/review/baseline.el --log agent/agent-shell-sidebar/review/fixes/identity-tests-baseline.log
```

These are the commands for the complete original probes:

```sh
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-1/ousterhout-probe.el --ert --source agent/agent-shell-sidebar/review/fixes/identity.el --log agent/agent-shell-sidebar/review/fixes/identity-ousterhout.log
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-1/torvalds-probe.el --ert --source agent/agent-shell-sidebar/review/fixes/identity.el --log agent/agent-shell-sidebar/review/fixes/identity-torvalds.log
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-1/contrarian-workflow.el --ert --source agent/agent-shell-sidebar/review/fixes/identity.el --log agent/agent-shell-sidebar/review/fixes/identity-workflow.log
```

The first line of `identity-relevant-reviewers.log` records the exact Emacs argument list for the selected original probes.
This command replays that argument list:

```sh
python3 - <<'PY'
from pathlib import Path
import json
import subprocess
log = Path('agent/agent-shell-sidebar/review/fixes/identity-relevant-reviewers.log')
command = json.loads(log.read_text().splitlines()[0].removeprefix('Command argv: '))
raise SystemExit(subprocess.call(command))
PY
```

`git apply --check` accepted the patch with exit 0.
The whitespace command produced no diagnostics. Its exit 1 records the difference between the two files.

```sh
git apply --check agent/agent-shell-sidebar/review/fixes/identity.patch
git diff --no-index --check agent/agent-shell-sidebar/review/baseline.el agent/agent-shell-sidebar/review/fixes/identity.el
```

The new regressions use temporary directories, transcript files, shell-state buffers, and windows.
The installed `agent-shell-buffers` implementation identifies the fixture owners.
The new regressions replace agent startup and display. The existing suite also runs its local Python ACP fixture.
No test contacts an external agent service or the running Emacs process.

Directory reuse retains the existing normalized-path comparison. This patch does not combine directory aliases for session reuse.
Configuration equality requires equivalent configuration contents. Custom makers that generate different opaque objects on every call remain outside the tested contract.
Filesystem identity comparisons remain separate from file deletion. The local fixtures do not establish an atomic guarantee against concurrent filesystem replacement.

The [integrity log](identity-integrity.log) confirms that the installed source, package declarations, and shared tests still match the frozen manifest.
This work changed only the isolated source, its regression file, and `identity` evidence artifacts.
No installed source, shared test, Desktop file, live Emacs state, or user configuration changed. No commit was created.
