# Round 1 — Ousterhout-inspired review

This review uses an abstraction and configuration-ownership perspective inspired by John Ousterhout. It does not claim his participation.

Reviewed the immutable frontend/core snapshots and installed agent-shell 0.74.3 / ACP implementation. The probes use the actual Codex client maker and ACP subprocess startup, connected only to the local Python fixture. No model prompt or external session was sent.

## Findings

### O1 — P1: Explicit ACP environment overrides the selected rollout's home

Frontend lines 642–655 bind `process-environment` and record the selected home on the buffer. They leave the chosen Codex configuration unchanged. The installed Codex client maker appends `agent-shell-openai-codex-environment` to its explicit client environment (`agent-shell-openai.el:213–219`). ACP prepends that explicit environment before the inherited environment (`acp.el:128–129`). Consequently a configured `CODEX_HOME` takes precedence over the selected rollout's home.

Reproduction: select a rollout from temporary `codex/`, configure ACP with `CODEX_HOME=other-codex/`, and start its agent-shell session. The actual child records `other-codex/`. Its explicit client environment also contains that other home. The sidebar marks the buffer as belonging to `codex/`, so its identity claim disagrees with the process it started. An adapter can therefore fail to find the selected session or load the same UUID from another store.

Test: `ousterhout-selected-home-overrides-configured-acp-home`. The desired invariant fails on the baseline.

Suggested correction: bind the home to the per-session configuration/client-maker boundary. Ensure the selected home wins in the client's explicit environment while retaining all other configured environment and authentication settings. Do not mutate the global Codex configuration.

### O2 — P1: Agent-shell reload loses a custom Codex home

Frontend lines 652–655 retain the selected home only in a dynamic environment binding and a sidebar buffer-local marker. `agent-shell-reload` calls `agent-shell-restart`, which kills that buffer and starts another using the stored configuration (`agent-shell.el:1506–1527`). The configuration does not preserve the selected home.

Reproduction: the ambient process environment points to `ambient/`, while the sidebar selects `codex/`. The first real fixture subprocess correctly receives `codex/`. After calling the actual noninteractive `agent-shell-reload`, the replacement child receives `ambient/` for the same session UUID. Both processes complete local `session/load`; the fixture deliberately accepts arbitrary UUIDs, so its successful reply does not conceal the independently recorded environment mismatch.

Test: `ousterhout-reload-preserves-selected-home`. The initial-child assertion passes; the replacement-child assertion fails.

Suggested correction: make the selected home part of the configuration that survives reload, restart, and deferred client creation. A buffer-local annotation alone cannot carry this contract across buffer replacement. The correction can share the per-session configuration work for O1.

### O3 — P2: A different home's matching buffer hides the reusable buffer

Frontend lines 646–651 ask the shared core for one session/configuration/directory match, then reject that single buffer if its Codex home differs. The core returns the first match (`baseline-1-agent-shell-sidebar.el:602–635`). It does not know about Codex homes.

Reproduction: two buffers have the same session UUID, resolved Codex config, and directory, with homes `other/` and `codex/`. The wrong-home buffer appears first in `agent-shell-buffers`. Requesting `codex/` starts a third buffer, even though the second buffer matches every identity component. The probe observes `starts=1` and the duplicate buffer as the return value.

Test: `ousterhout-reuse-searches-all-matching-home-candidates`. This is a controlled state fixture; it exercises the actual shared candidate selector and sidebar reuse logic, while replacing process creation with a counter.

Suggested correction: apply every identity constraint before selecting the first candidate. Normalize home directory representations consistently and include the preserved per-session configuration when comparing configurations.

## Replay

Run from `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-sidebar-codex/review/run-review.py \
  --probe agent/agent-sidebar-codex/review/round-1/ousterhout-tests.el \
  --log agent/agent-sidebar-codex/review/round-1/ousterhout-replay.log \
  --selector '"^ousterhout-"'
```

Baseline result: 3 tests, 3 expected-invariant failures. See `ousterhout.log` for the full evidence. These tests intentionally assert the corrected behavior and can be replayed unchanged against a fixed frontend with the runner's `--front` option.

The probes do not establish behavior of a real server when supplied the wrong home. They establish that the wrong store is passed to the actual ACP process and that a duplicate start occurs. No production files were edited during this review.
