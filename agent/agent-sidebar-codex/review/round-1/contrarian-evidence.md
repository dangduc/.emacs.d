# Round 1 — contrarian evidence skeptic

This review challenges test oracles and metadata assumptions. It is an analytical perspective, not an attributed review by a named person. Product sources were not edited for this round.

Frozen frontend: `../baseline-0-agent-sidebar.el`, SHA-256 `fe729bbdfe6c76c198611c4f347041d6983f080ed085e6fab739b7a6843739b0`.

## Findings

### CE-1 (P2): injected plugin inventories become session titles

`agent-sidebar--codex-user-text` at baseline lines 565–578 excludes several context wrappers but omits `<recommended_plugins>`. When this standalone injected block precedes the request, the first prompt preview becomes `<recommended_plugins>` and filtering by the actual request fails because the preview never contains it.

Executable evidence: `ce-plugin-inventory-is-context-not-the-first-prompt` fails with actual `<recommended_plugins>` versus expected `FIRST REQUEST`. A bounded, read-only structural sample of 100 recent native rollouts found 14 with this wrapper as their first accepted fallback; all 14 wrappers had a closing tag and an empty suffix. The observer records counts only: no paths or transcript text. See `contrarian-evidence-native-shapes.json` and its replay script.

Fix direction: exclude complete standalone plugin-context blocks. Preserve inline/quoted markup and any request following a closing tag in the same block.

### CE-2 (P2): a later event replaces the documented first prompt

`agent-sidebar--entry-preview` at baseline lines 703–705 promises the first prompt. The Codex parser at lines 600–615 independently captures an early `response_item` fallback, then prefers the first `event_msg.user_message` anywhere later in the prefix. Thus a first turn saved as response items and a later turn saved with events produces the second request as its title.

Executable evidence: `ce-first-prompt-survives-later-event-from-a-new-turn` fails with `SECOND REQUEST` versus `FIRST REQUEST`. The original feature test `codex-fork-keeps-first-session-meta-and-prefers-user-event-preview` explicitly expects this override, so it cannot detect the chronology bug. The independent oracle uses file order and the public helper's first-prompt contract, not that test's expected implementation.

This is a synthetic mixed-writer reproduction. The 100-file structural sample contained eight prefixes with both forms; their first lines matched. No mismatching real transcript is claimed. Resuming a response-only history with an event-producing writer is the trigger covered by the fixture.

Fix direction: select the earliest eligible user prompt across both record forms. Skip injected context before assigning it; retain event-only histories. Keep the first session metadata identity when inherited fork metadata follows it.

## Evidence and controls

Replay:

```sh
python3 agent/agent-sidebar-codex/review/run-review.py \
  --probe agent/agent-sidebar-codex/review/round-1/contrarian-evidence-tests.el \
  --log agent/agent-sidebar-codex/review/round-1/contrarian-evidence.log
```

Result: six tests, four passes and the two finding regressions fail. Independent passing controls cover event-only history, fork identity, and inline user markup. A mutation control disables context rejection and verifies that the first-prompt oracle then raises an ERT failure; the unmodified context-filter control passes.

The parser never launches a session. All test transcript text is synthetic. The native observer reads at most 262144 bytes per sampled file and emits structure counts only.
