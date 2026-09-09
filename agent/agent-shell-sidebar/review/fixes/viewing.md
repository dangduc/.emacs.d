T2R-1 isolated fix: the `o` command checks current attributes before opening a transcript. It uses the existing regular-file predicate to reject a FIFO, directory, or symbolic link. The missing/unreadable check remains first, and ordinary files still open in real View mode.

The [isolated source](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/fixes/viewing.el) copies Round 2 source SHA256 `629a0b61dc25a1b9766936b34b655aea9e1775c57f0f2d98ebb9ef00fea1b6ed` and adds two lines at the viewing entry point. The [patch](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/fixes/viewing.patch) targets `lisp/agent-shell-sidebar.el`. Canonical source and the frozen snapshot were not edited.

Validation on Emacs 31.1:

- All three [viewing regressions](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/fixes/viewing-tests.el) pass: regular-file content and View mode; missing/unreadable errors; and rejection of FIFO/directory/symlink replacements before file opening. [Log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/fixes/viewing-tests.log).
- Against the frozen source, the two controls pass and the nonregular-replacement regression fails. The test intercepts the unsafe open to avoid blocking this batch run. [Baseline log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/fixes/viewing-baseline.log).
- The separate bounded subprocess probe uses the real opener. Its regular control returns with View mode in 0.217 seconds; the replaced FIFO is refused in 0.215 seconds. On the frozen source, that FIFO run reached the four-second timeout. [Fixed subprocess log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/fixes/viewing-file-open.log).
- Byte compilation with `byte-compile-error-on-warn` enabled exits successfully. [Compile log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/fixes/viewing-compile.log).

Reproduce the behavioral checks from `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/fixes/viewing-tests.el --source agent/agent-shell-sidebar/review/fixes/viewing.el --ert --log agent/agent-shell-sidebar/review/fixes/viewing-tests.log
python3 agent/agent-shell-sidebar/review/round-2/torvalds-file-open.py --source agent/agent-shell-sidebar/review/fixes/viewing.el --log agent/agent-shell-sidebar/review/fixes/viewing-file-open.log
```

The attribute check and open are separate operations. A concurrent replacement between them remains possible. This change follows the existing parser's bounded precheck approach and does not add atomic file-opening machinery. Fixtures use temporary files and batch processes; no external agent service is involved.
