# Round 1 — process and filesystem correctness

This review uses a Linus Torvalds-inspired perspective: explicit ownership, process identity, and the behavior of the underlying API. It does not claim his participation.

Reviewed the immutable `baseline-0-agent-sidebar.el` and installed Emacs terminal contracts. No product files changed.

## R1-T1 — P2: equivalent Codex home paths start duplicate processes

Source: `baseline-0-agent-sidebar.el:633–640`, especially line 640; identity comparison is at lines 516–521.

`file-truename` resolves aliases but preserves a trailing directory separator. Thus `/tmp/home/` and `/tmp/home` name the same directory but create different terminal identities. Resume once with the former home, change the setting to the latter, refresh, then resume the same UUID and working directory: another process starts.

Executed `torvalds-codex-home-trailing-slash-reuses-process`. The positive control resumes twice with an identical home and sees one process. Changing only the trailing slash yields:

```text
TRAILING-SLASH: starts=2, live-children=2, both homes equal by file-equal-p=t
```

The final assertion expects one process and fails. The probe uses the real dispatcher and real local `cat` processes behind a ghostel API fixture. Metadata is supplied directly to isolate identity handling; it does not test native ghostel rendering.

Normalize the canonical home as a directory before building its identity. Use the same normalization for ACP home identity. Preserve isolation between genuinely different homes.

## R1-T2 — P2: term fallback reuses a buffer after the dispatcher rejected reuse

Source: `baseline-0-agent-sidebar.el:502–504`. Relevant callers: lines 527–532 and 945.

`make-term` reuses an existing named buffer and its live process. Its name is therefore not just a display label. The sidebar has already decided that a new process is required, but then passes a non-unique label to `make-term`.

Two executable probes expose this:

1. `torvalds-term-new-session-really-starts-new-process` calls the public `agent-sidebar-new-session` twice on a Claude row, with ghostel and vterm unavailable. Both calls return the same buffer and process. Pressing `N` the second time therefore does not create a new session.
2. `torvalds-term-fallback-distinguishes-resume-id-prefix-collision` resumes two different IDs that share their first eight characters. The label truncation collides. The second call gets the first child's buffer and rewrites its recorded session identity to the second ID. The process still has the first ID in its actual command.

Actual observations:

```text
TERM-NEW: same-buffer=t, same-process=t
TERM-COLLISION: same-buffer=t, same-process=t
child command ends with: "fixture" "12345678-1111-7111-8111-111111111111"
recorded session becomes: "12345678-2222-7222-8222-222222222222"
```

Both probes use real Emacs `term` processes. Children only execute local `cat` or `sleep`; no agent is contacted. Allocate a unique term buffer when the launcher creates a process, and leave intentional session reuse in `agent-sidebar--visit-terminal`.

## Replay and limits

```sh
python3 /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/run-review.py \
  --probe /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-1/torvalds-tests.el \
  --log /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-1/torvalds.log
```

Baseline outcome: 3 tests run, 3 failures at the desired-behavior assertions. These failures reproduce the two findings above. The probes terminate their child processes, kill their temporary buffers, and remove their temporary directories.

ACP environment precedence and reload behavior are assigned to another reviewer. No claim is made here about real provider authentication, remote session availability, or native terminal rendering.
