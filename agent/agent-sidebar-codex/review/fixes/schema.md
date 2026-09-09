# CE-3 content schema fix

Both native JSON providers now require a proper list before treating a candidate content block as an alist. An object-shaped `content` field is skipped; its dotted field pairs no longer abort metadata extraction. Later valid records remain available, and independently parsed session identity and CWD remain intact.

The product change is two guards: `listp` becomes `proper-list-p` in `agent-sidebar--content-text` and `agent-sidebar--codex-user-text`. No error handler was broadened, so file errors and read-hook failures retain their existing behavior.

Validation:

```sh
python3 agent/agent-sidebar-codex/review/run-review.py \
  --front lisp/agent-sidebar.el --core lisp/agent-shell-sidebar.el \
  --probe agent/agent-sidebar-codex/review/fixes/schema-tests.el \
  --log agent/agent-sidebar-codex/review/fixes/schema.log
python3 agent/agent-sidebar-codex/review/fixes/schema-mutations.py
```

All ten tests pass. They include the two Round 2 schema regressions, four independent cache/byte-boundary controls, and four added schema checks. The added checks cover valid JSON block objects in arrays, unsupported scalar/nontext members, Claude string content, and preservation of Codex identity/CWD when no usable prompt exists.

The source mutation controls restore each old guard independently. Each is rejected by its provider-specific regression at `wrong-type-argument listp`. Their logs intentionally contain one failing test each; the mutation runner succeeds only when both intended failures occur.

This fixes a synthetic malformed-schema reproduction. It does not assert that current native writers emit object-shaped content. No Desktop/runtime reload or compilation was performed by this delegated fix.
