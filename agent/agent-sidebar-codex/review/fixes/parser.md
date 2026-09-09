# CE-1 and CE-2 parser fixes

The frontend now skips leading injected context wrappers, including standalone plugin inventories, and retains the first eligible prompt in file order across `response_item` and `event_msg` records. Event-only histories remain supported. The first session metadata record still supplies the fork's own identity.

A request after a closed context wrapper in the same text block is retained. Inline, quoted, and unknown markup is retained. Context removal advances an offset through the original string so repeated wrappers do not require repeated suffix copies. Repository instruction headings retain their existing context-only treatment.

Product scope: only `agent-sidebar--codex-user-text` and `agent-sidebar--codex-parse` in `lisp/agent-sidebar.el`.

The original feature test `codex-fork-keeps-first-session-meta-and-prefers-user-event-preview` was renamed to `codex-fork-keeps-first-session-meta-and-first-user-preview`. Its final expectation changed from `Actual user event` to `Native Codex prompt`. The earlier response item contains the first user request; the later event must not replace it. Other expectations in that test, and historical frozen sources and logs, were preserved.

Validation:

```sh
python3 agent/agent-sidebar-codex/review/run-review.py \
  --front lisp/agent-sidebar.el --core lisp/agent-shell-sidebar.el \
  --probe agent/agent-sidebar-codex/review/fixes/parser-tests.el \
  --log agent/agent-sidebar-codex/review/fixes/parser.log
python3 agent/agent-sidebar-codex/review/run-review.py \
  --front lisp/agent-sidebar.el --core lisp/agent-shell-sidebar.el \
  --probe agent/agent-sidebar-codex/tests.el \
  --selector codex-fork-keeps-first-session-meta-and-first-user-preview \
  --log agent/agent-sidebar-codex/review/fixes/parser-original-oracle.log
python3 agent/agent-sidebar-codex/review/fixes/parser-mutations.py
```

All 14 independent parser checks and the amended original check pass. The two source mutation controls separately restore the original defective helper/parser; each is rejected by its specific finding regression. The mutation logs intentionally contain one failing test each. The mutation runner succeeds only when both failures occur at the intended assertions.

No Desktop synchronization, runtime reload, or compilation was performed by this delegated fix.
