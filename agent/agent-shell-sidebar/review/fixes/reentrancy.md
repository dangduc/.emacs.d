The isolated fix prevents a completed obsolete read from consuming a replacement parse queue.
An insertion hook can refresh discovery before the original queue head finishes.
The old callback previously removed the new head and left that transcript without metadata.
The independent probe reproduced that loss with zero pending files and a remaining placeholder row.

The base copy includes the transcript viewing guard.
Its SHA256 is `dc4ebbc29ae95c93c5bf705724488a24e053f57a68a90a47db89740c4c1b18f1`.
The base is preserved as `reentrancy-base.el`.
The frozen candidate is `reentrancy.el`, with SHA256 `4c2e73058c39fe01215d879ffc882005cb980f06f05d4a4e0f72c0461a7546b3`.
The patch applies to that base and reproduces the candidate exactly.
Its labels are `a/lisp/agent-shell-sidebar.el` and `b/lisp/agent-shell-sidebar.el`.

Each replacement queue now receives a distinct identity token.
Cleanup invalidates that token.
The worker records its owner and token before the first read.
Before it consumes a file, it checks the owner, sidebar mode, token, and exact queue cell.
The same checks protect the retry state and error-cache update.
A nested callback can advance the queue without the outer callback removing a second head.

The worker skips rendering after its work becomes obsolete.
Its cleanup schedules work only in a live owner that still uses sidebar mode.
A reentrant refresh retains its replacement timer and pending count.
A mode change or owner destruction cannot recreate the old work.

The cache commit also checks its original owner, token, cache table, and entry identity.
A completed obsolete read therefore cannot overwrite a replacement cache or a newer nested read result.
This guard does not require sidebar mode, so direct `--ensure-parsed` calls still cache metadata in ordinary buffers.
A read discarded after a refresh remains in the replacement queue and can run again.

The independent Kingsbury probe passed all four tests against the candidate.
Its previously failing read-hook schedule retained the newly discovered file, parsed its Fresh metadata, and published its preview.
The independent control that refreshes after a completed read also passed.
The injected discovery-error control emits its expected timer-error message and then recovers.

Eight additional regressions passed against both source and bytecode:

- A refresh keeps the original head queued under a new token and preserves the replacement timer.
- An exception after refresh does not consume the new queue's retry allowance.
- A major-mode change during a read leaves no cache, queue, timer, or render state.
- Sidebar mode reinitialization preserves its replacement cache and complete queue.
- Owner destruction during a read does not schedule work in another buffer.
- Nested real timer dispatch consumes only one queue head.
- A nested read's newer cache entry survives the outer read.
- Direct parsing outside sidebar mode retains its cache behavior.

The bytecode build passed with warnings treated as errors.
The parent will run the combined integration suite and obtain an independent recheck of the frozen candidate.
The test callbacks use the real `timer-event-handler` and standard `after-insert-file-functions` hook.
The fixtures contain temporary local transcripts.
They do not use live Emacs, external services, or real keyboard input.

This patch changes queue ownership checks and cache-commit checks.
It does not change the timer architecture or the snapshot construction budget.
The existing full-buffer publication cost remains.
Continuous refreshes can keep invalidating reads, so completion requires a period without further replacement of the queue.

The commands ran from `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-2/kingsbury-independent-probe.el --source agent/agent-shell-sidebar/review/fixes/reentrancy.el --ert --log agent/agent-shell-sidebar/review/fixes/reentrancy-independent.log
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/fixes/reentrancy-tests.el --source agent/agent-shell-sidebar/review/fixes/reentrancy.el --ert --log agent/agent-shell-sidebar/review/fixes/reentrancy-tests.log
```

The compile driver creates a temporary copy and discovers the installed dependency paths.
It compiles that copy with warnings treated as errors and runs the eight focused tests against its bytecode.

```sh
python3 agent/agent-shell-sidebar/review/fixes/reentrancy-check.py
```

The logs are `reentrancy-compile.log` and `reentrancy-compiled-tests.log`.

This work changed only `review/fixes/reentrancy*` artifacts.
It did not edit the integration source, canonical source, or frozen Round 2 source.
