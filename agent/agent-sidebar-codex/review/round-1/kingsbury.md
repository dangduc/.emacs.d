# Round 1: state transitions and ownership

Perspective: Kyle Kingsbury-inspired state-machine analysis. This is an analytical perspective, not a review by Kyle Kingsbury.

Reviewed the immutable `baseline-0-agent-sidebar.el` and `baseline-1-agent-shell-sidebar.el` in the parent review directory. No production code was changed.

## K1 — P1: a new ACP session's native rollout can be deleted while the session is open

Sources: `baseline-0-agent-sidebar.el:642-673`, `:675-684`, and `:948-950`; the deletion caller is `baseline-1-agent-shell-sidebar.el:1273-1278`.

`C-u N` starts Codex through agent-shell and saves its home. It does not set `agent-sidebar--agent-shell-file`. Only resuming an existing native row through `A` sets that path. The ownership predicate requires a saved path, so the new session's native rollout is unprotected when discovery later finds it.

The probe calls the actual `agent-sidebar-new-session` command, the installed agent-shell Codex configuration, and an offline ACP subprocess. The subprocess returns a valid UUID for `session/new`. The test creates the corresponding native rollout, discovers and parses it, marks it, confirms deletion, and invokes the actual deletion command. The live agent-shell still contains the same active UUID after the file disappears.

Observed:

```text
NEW ACP ownership: active-id="23456789-1234-7123-8123-123456789abc" native-marker=nil owned=nil
Deleted 1 transcript(s)
NEW ACP after delete: exists=nil retained-mark=nil active-id="23456789-1234-7123-8123-123456789abc"
```

Expected: retain the active session's rollout and deletion mark, matching the existing active-session deletion guard. Actual: the file and mark are removed.

Fix direction: derive native ownership from the actual ACP session UUID, Codex configuration, and Codex home. Saved paths can remain a fast path but cannot be the sole ownership record. Use bounded metadata for the file being deleted and indexed/open-buffer state; avoid parsing every rollout for each deletion. Preserve this identity across agent-shell restart/reload. This probe directly establishes the ACP case; it does not establish how to recover the unknown session UUID of a new CLI process.

## K2 — P2: activation proceeds after its parser refresh removes the selected entry

Sources: `baseline-0-agent-sidebar.el:340-344`, `:510-532`, and `:901-909`; the shared cache guard is `baseline-1-agent-shell-sidebar.el:405-417`.

The shared parser detects generation changes and refuses to cache an obsolete result. It still returns that result. The frontend activation wrapper does not recheck the selected entry or generation after parsing. It can therefore launch a session from metadata whose row disappeared during the read.

The probe wraps the real Codex parser with a reentrant callback that removes the file and calls `agent-sidebar-refresh` after metadata is read. The callback represents a provider/read extension that yields or refreshes. It preserves the real parser result and uses the actual visit command; only the terminal launcher is replaced with a recorder.

Observed: `REENTRANT action: removed=t current-entry=nil launches=1`. The recorder receives `codex resume <removed-session-UUID>` despite the selected file no longer existing in discovery.

Expected: abort an activation whose entry was removed or replaced during metadata retrieval. Actual: launch using the obsolete result. This is a deterministic reentrancy probe, not evidence that the stock local JSON reader yields at this exact point in normal operation.

Fix direction: after metadata retrieval, verify that the activation still belongs to the same live sidebar and current provider entry. A refresh that leaves the same identity intact can be accepted; removal or replacement must stop the action.

## Evidence and replay

`kingsbury-tests.el` contains three ERT probes; `kingsbury-acp-fixture.py` handles only offline ACP initialization and session creation/loading. No model prompts are sent.

```sh
python3 /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/run-review.py \
  --probe /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-1/kingsbury-tests.el \
  --log /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-1/kingsbury.log \
  --selector '"^kingsbury-"'
```

Baseline result: one control passed and both invariant assertions failed. The control verifies that the existing path-based owner guard recognizes a resumed native file. The full output is in `kingsbury.log`.
