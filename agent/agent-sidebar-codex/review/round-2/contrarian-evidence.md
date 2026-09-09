# Round 2 — contrarian evidence skeptic

This review uses new schema and cache-boundary oracles against the frozen `round-2-agent-sidebar.el` and `round-2-agent-shell-sidebar.el` sources. It does not replay the round-one parser suite.

## CE-3 (P2): an unexpected content shape blocks an otherwise valid session

Both `agent-sidebar--codex-user-text` (snapshot line 619) and `agent-sidebar--content-text` (line 411) use `listp` to admit a content block before calling `alist-get`. Emacs `listp` accepts a dotted pair. If a JSON user record has object-shaped `content` where an array is expected, iteration supplies field pairs such as `(type . "input_text")`. `alist-get` then signals `wrong-type-argument` instead of skipping that unsupported record.

Independent Codex and Claude fixtures contain valid identity/CWD metadata and a later valid request after the unsupported record. Both fail to return metadata. This prevents normal activation of that transcript; it is not a claim that the entire sidebar crashes. The existing bounded parse retry cannot repair a persistent schema mismatch.

This is malformed-schema resilience, demonstrated with synthetic JSON. No claim is made that the current Codex or Claude writers normally emit this shape. The useful invariant is that one unsupported content block must not discard independently valid session identity and later usable metadata.

Fix direction: validate that each candidate block has a proper list shape before alist access. Continue scanning later blocks/records. Preserve valid JSON block objects inside content arrays and Claude's supported string content; do not catch unrelated file/read-hook failures as schema noise.

## New passing controls

- Completing an initially incomplete JSONL user record invalidates the successful cache entry whose preview was empty. An unchanged file reuses that entry without invoking the parser.
- A multibyte UTF-8 user record crossing the 262144-byte prefix does not leak a partial preview or lose the preceding session identity.
- The 200-character preview bound remains present through parser dispatch, cache storage, and activation revalidation.
- A mutation control disables the preview bound. The independent cache/activation oracle then raises an ERT failure; the unmodified control passes.

Replay:

```sh
python3 agent/agent-sidebar-codex/review/run-review.py \
  --front agent/agent-sidebar-codex/review/round-2-agent-sidebar.el \
  --core agent/agent-sidebar-codex/review/round-2-agent-shell-sidebar.el \
  --probe agent/agent-sidebar-codex/review/round-2/contrarian-evidence-tests.el \
  --log agent/agent-sidebar-codex/review/round-2/contrarian-evidence.log
```

Result: six tests; four controls pass and the two cross-provider schema regressions fail. All tests express desired behavior. No diagnostic test asserts that defective product behavior should remain.

The retained `contrarian-evidence-fixture-initial.log` also shows two fixture failures: the cache helper omitted the discovery attribute table. Initializing that table corrected the fixture; the final log is authoritative for this review. No product change was made to accommodate those fixture failures.

All transcripts are synthetic. No session or model prompt was launched. Product files were not edited before this report was written.
