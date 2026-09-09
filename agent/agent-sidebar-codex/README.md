# Codex sidebar support

The [standalone rename](../agent-sidebar-rename/README.md) records the current file structure and validation.
The [two-round code review](review/README.md) records the earlier provider fixes.
The validation logs below retain the initial feature-delivery evidence.

The [updated Desktop file](/Users/ducnguyen/Desktop/agent-sidebar.el) supports native Codex rollouts and Codex through agent-shell.
The [installed copy](/Users/ducnguyen/.emacs.d/lisp/agent-sidebar.el) is compiled and loaded in Emacs.
The current delivery is one `agent-sidebar.el` file with direct definitions.

| Context | Action |
| --- | --- |
| Codex CLI row, `RET` | Resume the session in ghostel |
| Codex CLI row, `A` | Resume the same session through agent-shell |
| Codex CLI row or repository group, `N` | Start a new Codex CLI session in ghostel |
| Codex CLI row or repository group, `C-u N` | Start a new Codex agent-shell session |
| Codex Markdown transcript under agent-shell, `RET` | Resume through agent-shell with the Codex configuration |
| Agent-shell menu, `x` | Start Codex through agent-shell |

Open the sidebar with `M-x agent-sidebar-toggle-sidebar`.
The agent-shell action is also available as `M-x agent-sidebar-visit-in-agent-shell`.

## Discovery and resumption

The default providers are now `(agent-shell claude-cli codex-cli)`.
Native rollouts come from `sessions/YYYY/MM/DD/rollout-*.jsonl` below `agent-sidebar-codex-home`.
That setting defaults to `CODEX_HOME`, or `~/.codex/` when the environment variable is absent.
The scanner excludes archives, directory symlinks, file symlinks, named pipes, and unrelated directories.
Native rollouts can originate in the CLI or another Codex interface that uses the same local store.

The parser reads at most 262,144 bytes and requires a UUID from the first complete `session_meta` record.
The resumable thread ID comes from `payload.id`; the separate runtime `session_id` is not used.
Later metadata from an inherited history cannot replace the selected thread's identity or directory.
Prompt text cannot supply the session ID.
The preview uses the first eligible user prompt in file order across event and response records.
Known injected context is skipped, including plugin inventories; user text after a closed wrapper is retained.
Cached previews stop at 200 characters, including an ellipsis.
Sidebar filtering searches that preview and the existing metadata fields; `o` opens the complete transcript.
The model comes from `turn_context`.

The CLI path passes `resume` and the UUID as separate arguments to ghostel.
It runs in the recorded working directory and inherits the user's normal Codex configuration.
This follows the installed CLI 0.153.4 help and the [official resume reference](https://learn.chatgpt.com/docs/developer-commands?surface=cli#codex-resume).
A custom `agent-sidebar-terminal-function` still takes precedence.

Each discovered entry records its Codex home.
CLI reuse matches provider, session ID, working directory, and Codex home.
Agent-shell reuse also checks the Codex configuration and home.
Sidebar-launched children receive the matching `CODEX_HOME`; Emacs's parent environment is unchanged.
A per-session ACP configuration preserves that home across delayed startup and reload while retaining other authentication and environment settings.
The sidebar retains deletion marks for rollouts owned by its open terminals or identified Codex agent-shell sessions.
Native ACP ownership is checked by current UUID and home, including after new-session creation and buffer replacement.
`N` validates current row metadata before choosing a directory.
Group actions require one provider, agent, working directory, and home; ambiguous or stale groups require a more specific selection.
Local terminal actions reject remote directories before filesystem probes.
Terminal display failures clean up newly created children and retain pre-existing custom-runner buffers.

## Adapter and validation

Installed `@agentclientprotocol/codex-acp` 1.10.0 alongside the existing Claude ACP adapter.
The adapter exposes the native Codex thread ID through ACP `session/load`.
Its installed implementation maps that ID to `thread/resume`, as described by the [adapter project](https://github.com/agentclientprotocol/codex-acp).

The [31 tests](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/tests.el) include the 22 existing merge regressions and nine Codex checks.
They pass with [Emacs 31 source](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/final-source-tests.log), [Emacs 31 bytecode](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/final-compiled-tests.log), and [Emacs 30 with source dependencies](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/final-emacs30-tests.log).
[Compilation](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/compile.log) treats warnings as errors and passes.

The tests run agent-shell's actual Codex client against a local ACP fixture.
They verify `session/load`, `session/new`, recorded directory, child home, reuse, and ownership of the native rollout.
The [installed adapter check](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/adapter-smoke.json) uses an isolated home and verifies a real ACP initialize response with `loadSession` support.

The [running-Emacs check](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/live-check.eldata) uses a real idle timer and native ghostel terminal.
It verifies parsed metadata, rendered prompt and model, exact child arguments, directory, home, and reuse of the live process.
The ghostel child is a local recorder, so this check does not submit a Codex prompt.
No provider authentication or remote model response was exercised.

Initial discovery and explicit redraw still scale with the number of entries.
Metadata outside the bounded prefix remains unavailable.
Ownership tracking cannot identify every Codex process launched outside the sidebar.
The [previous merge notes](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-merge/README.md) retain the shared renderer's performance measurements and limits.

## Repeat checks

Use the [standalone runner](../agent-sidebar-rename/README.md#replay) for the current file.
These commands describe the earlier two-file delivery:

```sh
python3 agent/agent-sidebar-merge/run.py --compile --log agent/agent-sidebar-codex/compile.log
python3 agent/agent-sidebar-merge/run.py --probe agent/agent-sidebar-codex/tests.el --log agent/agent-sidebar-codex/tests.log
python3 agent/agent-sidebar-merge/run.py --compiled --probe agent/agent-sidebar-codex/tests.el --log agent/agent-sidebar-codex/compiled-tests.log
python3 agent/agent-sidebar-merge/run.py --emacs /Applications/Emacs-30.app/Contents/MacOS/Emacs --source-dependencies --probe agent/agent-sidebar-codex/tests.el --log agent/agent-sidebar-codex/emacs30-tests.log
python3 agent/agent-sidebar-codex/adapter-smoke.py
```
