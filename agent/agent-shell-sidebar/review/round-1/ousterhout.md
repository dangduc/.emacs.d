Round 1 review through a John Ousterhout-inspired lens: state ownership, lifecycle boundaries, and duplicated decisions. This is an analytical perspective, not an attribution to Ousterhout.

Reviewed [source](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el), the existing tests, README, and installed agent-shell startup/session code. The source and saved baseline both had SHA256 `ef3848084626c7ede22c2947bfdb16b1901e6201bab735bc96caba84356973a7`.

The [independent probe](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/ousterhout-probe.el) has three assertions of expected behavior. All three fail against the baseline, as recorded in the [log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/ousterhout-probe.log). Run this command from `/Users/ducnguyen/.emacs.d` to reproduce each finding:

```sh
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-1/ousterhout-probe.el --ert --source agent/agent-shell-sidebar/review/baseline.el --log agent/agent-shell-sidebar/review/round-1/ousterhout-probe.log
```

The command exits with status 1 because the expected-behavior assertions fail. Fixtures use temporary files and buffers. No external agent service or live Emacs process is involved.

**OUSTERHOUT-1 — P2: A completed fallback session still matches its old resume request.**

Location: [agent-shell-sidebar.el:427](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:427), especially the unconditional alternative at line 428.

Trigger: resume `old-session` using an agent that does not advertise session loading or resumption. Installed agent-shell falls back to `session/new`. Its success callback sets the active session ID to `new-session` and leaves `:resume-session-id` as `old-session`. Activating the old transcript again therefore selects the new conversation.

Expected: once an active session ID exists, reuse must match that ID. The pending resume ID can identify a connecting shell before an active ID exists. Actual: the same shell matches both old and new session IDs.

Evidence: the shared command above runs `ousterhout-live-request-id-is-not-current-after-fallback`. It executes the real installed `agent-shell--initiate-session` fallback and real `session/new` success callback, with a local response replacing transport. It reports `actual-session="new-session" retained-request="old-session" old-row-reuses-new=t`. The probe first verifies that the connecting shell matches the requested ID and then verifies that the completed shell matches its actual ID.

Rationale: the lookup combines state from two lifecycle phases without defining which phase owns session identity. Suggested fix: prefer a nonempty active session ID and consult the pending resume ID only while no active session ID exists.

**OUSTERHOUT-2 — P3: Updating one sidebar's shared cache removes another sidebar's parsed rows.**

Location: [agent-shell-sidebar.el:146](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:146), with the snapshot signature check at [line 303](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:303).

Trigger: sidebar A parses a transcript. The transcript grows without changing its header. Sidebar B refreshes and parses the new signature. A redraws after a fold or filter command. A still owns the old file-attribute snapshot, but B has replaced the sole shared cache entry for that path.

Expected: A retains its parsed snapshot metadata, or schedules work to restore it. Actual: A renders the agent as `Unknown` and the preview as an ellipsis, with both its parse queue and parse timer nil. With automatic refresh disabled, those placeholders remain until a manual refresh.

Evidence: the shared command runs `ousterhout-global-cache-preserves-other-buffer-metadata`. Both sidebars initially verify the same `First prompt` metadata. After B parses the append, A reports `Unknown`, `…`, `pending=nil timer=nil` and fails its preview assertion.

Rationale: metadata is process-global while the signatures and work queue that validate it are buffer-local. One buffer can silently invalidate another's completed work. Suggested fix: keep parsed metadata with the buffer's snapshot, make the cache buffer-local, or retain entries by path and signature so concurrent snapshots remain valid. Redraw should retain its existing no-disk-access behavior.

**OUSTERHOUT-3 — P2: Configuration resolution and shell reuse disagree about agent identity.**

Location: [agent-shell-sidebar.el:430](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:430), compared with the unique substring match at [line 415](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:415).

Trigger: a transcript records `Claude Code`, and the only configured agent has the name `Claude`. The sidebar accepts that unique substring match and starts a shell. Pressing RET on the same transcript again starts another shell, including while the first is connecting.

Expected: the second activation reuses the shell started from the same resolved configuration, directory, and session ID. Actual: startup accepts the substring match but reuse requires an exact display-name match, so every activation can create another shell.

Evidence: the shared command runs `ousterhout-substring-agent-resolution-reuses-started-session`. It verifies that the real resolver selects the configuration, invokes the real visit command twice, and reports `recorded-agent=Claude Code resolved-agent=Claude visits=2 starts=2`. Only process creation and display are replaced.

Rationale: two functions separately define what an agent name means. Suggested fix: resolve the configuration once and use that resolved identity for both existing-shell lookup and startup. Preserve ambiguity handling when resolution requires user selection.
