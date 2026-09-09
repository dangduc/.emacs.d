This is an independent Kingsbury-inspired addendum to Round 2. It is not a third review round or a statement by Kyle Kingsbury.
I did not implement the sidebar scheduling changes.

I reviewed `round-2/reviewed.el`, SHA256 `629a0b61dc25a1b9766936b34b655aea9e1775c57f0f2d98ebb9ef00fea1b6ed`.
Four fresh deterministic controls use temporary transcripts, ordinary owner buffers, and the real `timer-event-handler`.
Three pass. One establishes **R2-K1: a resumed read consumes an entry from a replacement parse queue**.

**R2-K1 — P2, queue ownership after a callback.** Source locations are `reviewed.el:435` and `reviewed.el:450–455`; refresh replaces the queue at `reviewed.el:1235–1236`.
A one-shot standard `after-insert-file-functions` callback creates a newer temporary transcript and refreshes its sidebar owner.
The callback runs while the original queue head is being read. Refresh queues the new transcript first.
The original read resumes and unconditionally pops that new queue head, although it parsed a different file.
After every owned parse timer and staged render finishes, the new transcript has no cached header or visible preview, and the pending count is zero.
The test makes no direct writes to sidebar queues, caches, timer slots, or rendering state.

The control performs the same file creation and refresh immediately after the read finishes. It publishes the new metadata and preview and ends with zero pending work.
The difference therefore depends on refresh occurring inside the read callback, rather than transcript contents, timestamps, sorting, or the drain helper.
The coordinator accepted R2-K1 and delegated its fix. The fix must check that resumed work still owns the queue before consuming an entry or changing its retry state.
Source inspection distinguishes this from the baseline: `baseline.el:358` consumes the queue head before calling the reader. The new failure-retention logic moves consumption after the callback. The full baseline was not run with these staging controls because it predates that staging API.

| Control | Source areas | Result |
| --- | --- | --- |
| Refresh during a standard read callback discovers a newer transcript | 422–459; 1226–1238 | Fails: the new queue head disappears without parsing; final preview is absent and pending is zero |
| Same discovery update after the read returns | 1226–1238 | Passes: the new header and preview appear |
| Transient discovery error while a 320-file stage is incomplete, followed by the next automatic refresh | 486–497; 979–1013; 1226–1238 | Passes: 319 pending headers and the stage survive the error; the next automatic refresh completes all work; an old cancelled parse timer has no effect |
| Kill an owner and recreate a sidebar with its exact buffer name before dispatching old parse and refresh timer objects | 402–420; 486–513 | Passes: old callbacks leave both replacement timers and its pending count unchanged; the replacement completes |

The discovery-error control also records a limit: the failed scan cancels the current parse continuation.
A refresh timer remains owned and restores progress on its next callback. The test dispatches that callback without waiting for its wall-clock delay.
This establishes recovery, not uninterrupted progress after a scan failure. It is not counted as another finding.

The failing schedule injects a supported file hook that calls refresh; it does not establish how often ordinary sidebar usage reaches that interleaving.
The controls do not establish interactive redisplay latency, unfinished key-sequence behavior, or arbitrary recursive-edit histories.
No terminal editor, live Emacs, real user transcript, or external service was used.
No product source was edited.

Evidence is `kingsbury-independent-probe.el`, `kingsbury-independent-probe.log`, and `kingsbury-independent-provenance.json` in this directory.
The log records the loaded source before and after ERT. The overall command exits 1 because R2-K1 fails its required-behavior assertion.

```sh
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-2/kingsbury-independent-probe.el --source agent/agent-shell-sidebar/review/round-2/reviewed.el --ert --log agent/agent-shell-sidebar/review/round-2/kingsbury-independent-probe.log
```

Independent Round 2 fix recheck: **R2-K1 is fixed in the tested candidate**.
The candidate is `fixes/reentrancy.el`, SHA256 `4c2e73058c39fe01215d879ffc882005cb980f06f05d4a4e0f72c0461a7546b3`.
The exact same four probes now pass, with no changes to their fixture, assertions, or dispatch order.
The previously lost new queue head remains queued, receives its `Fresh` header, and displays its preview before work drains to zero pending.
The three controls continue to pass. The original frozen-source 3/4 result and log are preserved.

I inspected the candidate diff against `fixes/viewing.el`.
The queue receives a fresh identity at line 407, and cleanup invalidates it at line 534.
At lines 463–479, both retry/error handling and successful queue consumption require the original generation and queue cell.
The cache commit at lines 391–400 also requires the original owner, generation, cache table, and entry.
These checks prevent an obsolete read from modifying replacement state. The callback's cleanup can still arm current work for a surviving sidebar owner.
No remaining defect was established by this replay and narrow source inspection.

The recheck log is `kingsbury-independent-recheck.log`; it records the candidate source before and after ERT.
`kingsbury-independent-provenance.json` now also records the candidate and recheck log hashes.
The recheck command exits 0:

```sh
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-2/kingsbury-independent-probe.el --source agent/agent-shell-sidebar/review/fixes/reentrancy.el --ert --log agent/agent-shell-sidebar/review/round-2/kingsbury-independent-recheck.log
```

This closes the independent R2-K1 follow-up within Round 2. It does not add a review round or extend the interactive coverage stated above.
