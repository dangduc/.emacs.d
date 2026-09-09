# K1 fix: native rollout ownership

Changed only the ownership region of `lisp/agent-sidebar.el`: added `agent-sidebar--codex-buffer-session` and `agent-sidebar--codex-owned-file-p`, then connected the latter to the existing path and alias guard.

The guard now checks open Codex agent-shell buffers by their actual active UUID and known Codex home. A pending resume UUID applies only when no active UUID exists. The home comes from the client/config helper added by the configuration fix, so actual agent-shell reload retains this association without either legacy buffer marker.

For a selected native file, the guard first locates a matching active home. It then re-reads that file's bounded metadata prefix once. It does not scan or parse other rollouts. After reading, it checks the current session/home again in case a hook changed or killed the buffer. If metadata cannot establish ownership under an active Codex home, deletion retains the mark with an error. Existing path guards, aliases, and the shared Markdown transcript guard remain in place.

Validation: all nine ERT tests in `ownership-tests.el` pass against the current source. Tests cover actual offline ACP session/new, actual agent-shell reload with both legacy markers absent, home/config/UUID negative controls, active-versus-pending transitions, a state change during metadata reading, fresh metadata despite the cache, one-file bounded reading, alias compatibility, and malformed-prefix deletion refusal.

Post-snapshot correction: after the first Round 2 sources were frozen, the helper gained a `file-exists-p` guard. This lets the existing deletion command clear a stale mark when the file is already absent. `ownership-absent-file-does-not-retain-stale-mark` verifies this behavior. The immutable Round 2 snapshots were not changed.

Replay:

```sh
python3 /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/run-review.py \
  --front /Users/ducnguyen/.emacs.d/lisp/agent-sidebar.el \
  --core /Users/ducnguyen/.emacs.d/lisp/agent-shell-sidebar.el \
  --probe /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/fixes/ownership-tests.el \
  --log /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/fixes/ownership.log \
  --selector '"^ownership-"'
```

The result log records nine passes and the retained-file/retained-mark outcome for the original K1 reproduction. No real model prompt, Desktop synchronization, live Emacs reload, or byte compilation was performed by this fix agent.

Limit: the added identity lookup applies to Codex through ACP. A new CLI process whose thread UUID is not yet known is not assigned a native rollout by this change. Existing path guards remain conservative: an explicit saved path can remain protected while its owner buffer is open.
