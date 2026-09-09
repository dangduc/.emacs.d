# Standalone agent-sidebar rename

`agent-sidebar.el` now contains the provider browser and its sidebar infrastructure.
Its variables and functions use direct definitions under `agent-sidebar`.
Its mode derives from `special-mode`.
The file contains no aliases or references to `agent-shell-sidebar`.

The previous implementation required a companion library, shared its variables through aliases, and inherited its mode and keymap.
Loading that Desktop file alone failed with `Cannot open load file: No such file or directory, agent-shell-sidebar`.
The [baseline log](before-load.log) records this failure.
This identifies a packaging failure, but does not establish the exact error from the user's session.

The rename preserves Codex CLI, Codex through agent-shell, Claude CLI, grouping, cache validation, timer cleanup, and deletion checks.
The former internal file parser is now `agent-sidebar--ensure-header`.
The provider entry validator remains `agent-sidebar--ensure-parsed`.
These functions take different arguments and retain separate definitions.

The installed old source and bytecode moved out of `lisp` into this evidence directory.
Init contains only the `agent-sidebar` package entry.
The restarted Emacs had loaded neither library before this change.
Its obsolete command autoloads were removed before the standalone library loaded.

## Files

- [Desktop delivery](/Users/ducnguyen/Desktop/agent-sidebar.el)
- [Installed source](/Users/ducnguyen/.emacs.d/lisp/agent-sidebar.el)
- [Init package entry](/Users/ducnguyen/.emacs.d/lisp/package-declarations.el:1478)
- [Desktop identity check](desktop-sync.json)

Use `M-x agent-sidebar-toggle-sidebar`.
Set options with their `agent-sidebar-` names, including `agent-sidebar-name` and `agent-sidebar-width`.
The old names are no longer configuration entry points.

## Evidence

The isolated runner places only `agent-sidebar.el` and its package dependencies on `load-path`.
The 123 existing provider tests use renamed identifiers in temporary copies.
Their assertions and mutation controls remain unchanged.
The original review files retain their historical names and results.
Four additional tests cover standalone loading, direct variables, custom window configuration, key bindings, and independent buffer state with timer cleanup.

| Check | Result |
| --- | --- |
| [Emacs 31 source](source-tests.log) | 127/127 |
| [Emacs 31 bytecode](compiled-tests.log) | 127/127 |
| [Emacs 30 source](emacs30-tests.log) | 127/127 |
| [Compilation, warnings treated as errors](compile.log) | Passed |
| [Running Emacs fixture](live-check.eldata) | Metadata displayed; ghostel arguments, directory, home, and process reuse matched |

The live check ran in restarted Emacs PID 58644 at recursion depth zero.
It loaded `/Users/ducnguyen/.emacs.d/lisp/agent-sidebar.elc`.
The local ghostel recorder received no model prompt.
The standalone variable target was `agent-sidebar-name`, the mode parent was `special-mode`, and the old feature was absent.
Native GUI automation was unavailable, so this run has no screenshot or manual keyboard verification.

The first replay exposed two harness errors, recorded in `initial-harness-tests.log`.
The temporary suite lacked a shared Python ACP fixture, and the new width assertion measured the text body rather than total window width.
The final runner copies the shared fixture and measures total width.
No product change was needed for these failures.

## Replay

Run from `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-sidebar-rename/run.py
python3 agent/agent-sidebar-rename/run.py --compiled --log agent/agent-sidebar-rename/compiled-tests.log
python3 agent/agent-sidebar-rename/run.py --emacs /Applications/Emacs-30.app/Contents/MacOS/Emacs --source-dependencies --log agent/agent-sidebar-rename/emacs30-tests.log
python3 agent/agent-sidebar-rename/run.py --compile --log agent/agent-sidebar-rename/compile.log
```

`consolidate.py` records how the frozen source pair became the standalone file.
It is migration evidence, not a build requirement.
