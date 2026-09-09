# Sidebar review tracker

This review predates the [provider merge](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/README.md).
The snapshots, findings, and logs below remain evidence for the reviewed version.

This review uses six analytical perspectives inspired by the named reviewers.
The reports do not represent statements by those people.
Each perspective includes code written and run to support its analysis.

Baseline: `ef3848084626c7ede22c2947bfdb16b1901e6201bab735bc96caba84356973a7`.
The source snapshot is [baseline.el](baseline.el).

## Review rounds

| Perspective | Round 1 | Round 2 |
| --- | --- | --- |
| John Ousterhout: boundaries and state ownership | [Complete](round-1/ousterhout.md) | [Complete](round-2/ousterhout.md) |
| Dan Luu: latency and measurement | [Complete](round-1/luu.md) | [Complete](round-2/luu.md) |
| Linus Torvalds: contracts and practical correctness | [Complete](round-1/torvalds.md) | [Complete](round-2/torvalds.md) |
| Kyle Kingsbury: interleavings and failure recovery | [Complete](round-1/kingsbury.md) | [Complete](round-2/kingsbury-independent.md) |
| Contrarian: challenge the test evidence | [Complete](round-1/contrarian-evidence.md) | [Complete](round-2/contrarian-evidence.md) |
| Contrarian: challenge the user workflow | [Complete](round-1/contrarian-workflow.md) | [Complete](round-2/contrarian-workflow.md) |

Both rounds are complete. Each used all six perspectives and executable checks.
Round 2 checked the combined fixes and found two additional defects.
Its [initial scheduling-author controls](round-2/kingsbury.md) were followed by an independent Kingsbury review.
The independent review found R2-K1 and confirmed its fix with the unchanged failing probe.
Six distinct workers contributed to Round 2; one worker covered both Ousterhout and Torvalds perspectives.

## Findings

Round 1 found 13 distinct defects. Round 2 found two more product defects.
An independent Kingsbury check found the second Round 2 defect during its follow-up.
All product fixes were delegated in isolated source copies.
The evidence review also repaired a test-runner isolation error.
The final combined suite passes all 131 tests in source and bytecode on Emacs 31.1.
All 131 also pass on Emacs 30.2.50 with dependency sources.
Round 2 reviews [this frozen source](round-2/reviewed.el).
Its SHA256 is `629a0b61dc25a1b9766936b34b655aea9e1775c57f0f2d98ebb9ef00fea1b6ed`.
The installed source and Desktop copy now match the final reviewed code.
Running Emacs has loaded its compiled library.
Final source SHA256: `4c2e73058c39fe01215d879ffc882005cb980f06f05d4a4e0f72c0461a7546b3`.

| ID | Severity | Finding | Fix owner | Status |
| --- | --- | --- | --- | --- |
| O1 | P2 | A stale resume ID matches a different active session | Identity | Fixed; final suite passed |
| O2 | P3 | One sidebar invalidates another sidebar's metadata cache | Metadata | Fixed; final suite passed |
| O3 | P2 | Accepted agent name aliases fail the reuse check | Identity | Fixed; final suite passed |
| T1 | P2 | Project path aliases bypass transcript deletion ownership | Identity | Fixed; final suite passed |
| T2 | P2 | Cached read errors survive restored file permissions | Metadata | Fixed; final suite passed |
| L1 | P2 | Every parse chunk redraws all rows, producing quadratic work | Scheduling | Fixed; final suite passed |
| L2 | P2 | A new idle period inherits the previous idle age | Scheduling | Fixed; final suite passed |
| K1 | P2 | A read-hook error loses one file and stops remaining work | Scheduling | Fixed; final suite passed |
| CE1 | P2 | A partial metadata line becomes a session ID | Metadata | Fixed; final suite passed |
| CE2 | P2 | Whitespace trimming changes a valid working directory | Metadata | Fixed; final suite passed |
| CW1 | P2 | A `.md` FIFO blocks parsing | Metadata | Fixed; final suite passed |
| CW2 | P2 | Toggle treats an ordinary window as a removable side window | Identity | Fixed; final suite passed |
| CW3 | P3 | Disabling auto-refresh still permits one pending scan | Scheduling | Fixed; final suite passed |
| R2-T1 | P2 | Opening a formerly regular transcript can block on a FIFO | Viewing | Fixed; independent recheck and final suite passed |
| R2-K1 | P2 | A reentrant refresh causes the parser to discard replacement queue work | Reentrancy | Fixed; independent recheck and final suite passed |

The evidence skeptic also identified two assertion gaps in the existing suite.
Removing the byte limit or the first-message boundary still passed all 33 original tests.
The independent controls detect both mutations. Those controls are included in the final regression checks.

Fix reports: [identity](fixes/identity.md), [metadata](fixes/metadata.md), [scheduling](fixes/scheduling.md), [viewing](fixes/viewing.md), and [reentrancy](fixes/reentrancy.md).
All 15 confirmed product findings are fixed.

## Final verification

- 131/131 on [Emacs 31 source](final-source-tests.log), [Emacs 31 bytecode](final-compiled-tests.log), and [Emacs 30 source](final-emacs30-tests.log).
- [Strict compilation](final-compile.log) completed without warnings.
- [Four parser mutations](round-2/contrarian-evidence.md) fail the intended assertions.
- The [independent R2-K1 replay](round-2/kingsbury-independent-recheck.log) now passes unchanged.
- The [independent viewing checks](round-2/contrarian-workflow.md) reject changed nonregular files and preserve View mode for regular files.
- [Final performance counters](final-scale.log) include work after parsing ends and all snapshot publications.
- [Live Emacs](../live-check.eldata) parsed and rendered 71 fixture transcripts with real idle timers. [Cleanup](final-live-cleanup.eldata) left no fixture owners or staging buffers.

The harness fixes preserve the incoming native compiler policy and scope deletion spies to fixture transcripts.
Their causal controls remain in the [evidence review](round-2/contrarian-evidence.md) and [workflow addendum](round-2/contrarian-workflow-native-policy.md).
Original failures remain available alongside passing rechecks.

## Limits and review artifacts

Initial refresh and explicit redraws remain synchronous; large previews increase their cost.
An unfinished prefix-key idle history could not run because isolated terminal Emacs startup failed.
That case remains unverified, rather than a confirmed or closed product finding.
Filesystem checks and opens remain separate operations.
There was no visual screenshot check or external-provider authentication test.
The [main README](../README.md) states measurement and runtime limits in detail.

[Final patch](final.patch) compares the reviewed baseline with the installed source.
[Implementation patch](../sidebar.patch) compares the original supplied file with the final source.
[Integrity manifest](final-manifest.json) records file hashes and validation outcomes.
No commit or pull request was created for this review.
