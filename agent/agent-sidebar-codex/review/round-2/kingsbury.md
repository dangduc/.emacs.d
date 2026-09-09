# Round 2: ownership across buffer replacement

Perspective: Kyle Kingsbury-inspired state-transition review, not a review by Kyle Kingsbury.

Reviewed the immutable `round-2-agent-sidebar.el` and `round-2-agent-shell-sidebar.el`. This round adds new lifecycle probes rather than only replaying Round 1. Known post-snapshot absent-file and terminal-cleanup corrections were excluded from findings.

## K3 — P1: reload during metadata reading loses the active owner

Source: `round-2-agent-sidebar.el:836-862`.

The new ownership guard records candidate buffer objects before reading the selected native rollout. After reading, it checks only those same buffer objects. `agent-shell-reload` kills its original buffer and starts a replacement. If a file hook reloads the session during the metadata read, the old object is dead and the replacement is absent from the candidate list. The guard reports that the file is unowned even though the replacement has the same active UUID and Codex home.

The executable probe starts the installed Codex ACP configuration with the offline fixture, waits for its session UUID, marks the matching rollout, and calls the actual deletion command. An `after-insert-file-functions` hook calls the actual `agent-shell-reload` command and waits for the replacement to initialize. The probe does not stub the parser, ownership predicate, agent-shell restart, or deletion operation.

Observed:

```text
Deleted 1 transcript(s)
RELOAD DURING READ: old-live=nil new-live=t file-exists=nil retained-mark=nil
```

Expected: retain the file and mark while the replacement session owns that UUID and home. Actual: delete the file and clear the mark. The controlled file hook establishes the reentrant transition; it does not claim that the stock configuration automatically reloads sessions during transcript reads.

Fix direction: retain the validated home scope across the bounded metadata read, then enumerate current open buffers again before deciding ownership. Compare the current active or pending UUID and current home, not the identity of the original Emacs buffer. Parse only the selected file once.

## Independent controls

- A buffer that changes to another Codex home during metadata reading no longer owns the original home's matching UUID. This negative control passed.
- Disabling only the new native ownership helper lets the actual deletion command remove an active fixture rollout. This mutation control passed and establishes that another unrelated guard does not hide deletion in this setup.

Three tests ran: two controls passed and the replacement-buffer invariant failed. Source and output are `round-2/kingsbury-tests.el` and `round-2/kingsbury.log`. No model prompts were sent.

Replay:

```sh
python3 /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/run-review.py \
  --front /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-2-agent-sidebar.el \
  --core /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-2-agent-shell-sidebar.el \
  --probe /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-2/kingsbury-tests.el \
  --log /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-2/kingsbury.log \
  --selector '"^kingsbury-r2-"'
```
