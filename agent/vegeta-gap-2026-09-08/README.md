The published Vegeta code omits several features and fixes from our local sidebar.
Replacing the local file with the current Vegeta file loses those behaviors.
Both implementations are standalone files with direct definitions, so the companion-library and alias problem is already absent in both.

On September 8, 2026, both repositories had the same `main` commit:
[8f20c25212ffe56db8ae3e9d177102b44655e4eb](https://github.com/jojojames/vegeta/commit/8f20c25212ffe56db8ae3e9d177102b44655e4eb).
The [dangduc fork](https://github.com/dangduc/vegeta/tree/8f20c25212ffe56db8ae3e9d177102b44655e4eb) has no additional commits on `main`.
The repository history contains the license commit and one source import, titled `Add vegeta.el`.
That history does not establish which local revision supplied the import or why individual local changes are absent.

| Compared item | Snapshot |
| --- | --- |
| Installed source | [agent-sidebar.el](/Users/ducnguyen/.emacs.d/lisp/agent-sidebar.el), 2,220 lines |
| Desktop source | [agent-sidebar.el](/Users/ducnguyen/Desktop/agent-sidebar.el), identical SHA-256 |
| Local SHA-256 | `cde79fdb7f8abd6ad86cd30684c858503b856afeb940a4f1daedcceaf6ae7f8b` |
| Vegeta source | [vegeta.el](https://github.com/jojojames/vegeta/blob/8f20c25212ffe56db8ae3e9d177102b44655e4eb/vegeta.el), 1,356 lines |
| Vegeta SHA-256 | `e90a5a82102663b57717a75c1627bbd35846663ab7224c573c65a472fa626a68` |
| Running Emacs | PID 58644 loads `/Users/ducnguyen/.emacs.d/lisp/agent-sidebar.elc` |
| Vegeta in running Emacs | Feature absent, library absent from `load-path` |

The source comparison normalizes `agent-sidebar` to `vegeta` and reads top-level Lisp forms.
It finds 100 local-only declarations, 13 Vegeta-only declarations, 61 changed common declarations, and 30 identical common declarations.
These counts include variables, faces, documentation, and functions. They are not feature counts.
The 13 Vegeta-only declarations are internal helpers with local equivalents or replacements.
I found no additional user command in Vegeta that our local implementation lacks.
The [inventory](/Users/ducnguyen/.emacs.d/agent/vegeta-gap-2026-09-08/inventory.json) and [normalized diff](/Users/ducnguyen/.emacs.d/agent/vegeta-gap-2026-09-08/normalized.diff) preserve the comparison.

The following table separates shared features from missing local behavior.

| Area | Local sidebar | Current Vegeta |
| --- | --- | --- |
| Basic browser | Agent-shell and Claude discovery, grouping, folding, navigation, marks, and transcript opening | Present |
| Native Codex sessions | Discovers rollouts, resumes through ghostel, and scopes sessions by Codex home | No `codex-cli` provider or Codex home configuration |
| Codex through agent-shell | `A` resumes a native Codex rollout through ACP; `N` and prefix behavior start sessions | No dedicated bridge or new-session command |
| Filtering | `/` matches transcript metadata and paths | No filter command |
| Group configuration | `G` changes and validates grouping | Custom variable only; Evil `G` goes to the last line |
| Model grouping | Uses the parsed model, then the agent label | Uses the agent label for the `model` group |
| Project discovery | Includes the opening directory and project; remote scanning requires configuration | Known roots only; no remote-scan option |
| Repository identity | Uses known roots or recorded directories | Can guess a directory by replacing encoded hyphens with slashes |
| Packaging | Local source and review artifacts; declared agent-shell minimum `0.74.3` | Public `vegeta` namespace, GPLv3 license file, declared minimum `0.60` |
| Tests and docs in repository | Local review suites exist outside the product file | Git tree contains only `LICENSE` and `vegeta.el` |

Vegeta still has a generic agent-shell provider that can select a Codex configuration.
The missing Codex feature is native rollout discovery and the dedicated CLI/ACP bridge.
Our implementation of that bridge also preserves the selected home across ACP reload and distinguishes session ownership across homes.
The relevant local code starts at [Codex discovery](/Users/ducnguyen/.emacs.d/lisp/agent-sidebar.el:1541) and [ACP configuration](/Users/ducnguyen/.emacs.d/lisp/agent-sidebar.el:1670).

I ran the same 13 focused checks against both sources in isolated Emacs 31 processes.
The local source passed 13 checks. Vegeta passed two shared-feature checks and failed the 11 checks for local behavior.
These checks deliberately target the gaps. Their pass rates do not measure overall package quality.
The two shared checks cover ordinary Claude metadata, date grouping, mode inheritance, and navigation bindings.

| Reproduced gap | Local result | Vegeta result |
| --- | --- | --- |
| Codex provider and additional commands | Provider and four commands exist | Codex provider absent; source inventory also shows all four commands absent |
| Three redraws after discovery | Zero provider discovery calls | Three provider discovery calls |
| Repeated Claude activation | One terminal launch | Two terminal launches |
| Delete a marked file owned by an open agent-shell | File and mark retained | File deleted |
| Change file size but preserve modification time, then activate | Updated preview returned | Old preview returned |
| Model appears in the assistant record after the first user record | Model returned | Model absent |
| Text follows a tool-result block | User text returned | Preview absent |
| First prompt has 5,000 characters | Cached preview has 200 characters | Cached preview has 5,000 characters |
| Two sidebar buffers have different parse work | Queue remains local to its buffer | Second buffer sees the first buffer's queue |
| Two Claude sessions start within the recovery interval | No guessed session | Nearest session selected |
| Open a transcript with `o` | View mode enabled | View mode absent |

The checks use synthetic files and stub terminal or session entry points.
The deletion check removes only its temporary fixture under Vegeta.
The terminal check measures dispatch and reuse decisions, without a real CLI process.
No real user transcript or model service participates in these checks.
The code is in [gap-tests.el](/Users/ducnguyen/.emacs.d/agent/vegeta-gap-2026-09-08/gap-tests.el).
The [local log](/Users/ducnguyen/.emacs.d/agent/vegeta-gap-2026-09-08/local-tests.log) and [Vegeta log](/Users/ducnguyen/.emacs.d/agent/vegeta-gap-2026-09-08/vegeta-tests.log) contain each assertion and result.

Source inspection identifies additional differences that this focused suite does not exercise.
Vegeta uses a global repeating parse timer and performs a complete discovery during redraw.
Our [parse loop](/Users/ducnguyen/.emacs.d/lisp/agent-sidebar.el:503) schedules work per buffer, stops for pending input, and guards against obsolete reads.
Our renderer builds a snapshot incrementally and publishes it after completion.
Initial discovery still scales with the number of files. This comparison does not establish a latency bound for either implementation.

Vegeta's [agent-shell activation](https://github.com/jojojames/vegeta/blob/8f20c25212ffe56db8ae3e9d177102b44655e4eb/vegeta.el#L475) matches live buffers by session ID alone.
Our [activation code](/Users/ducnguyen/.emacs.d/lisp/agent-sidebar.el:670) also considers configuration, directory, and pending resume state.
It preserves the transcript directory through asynchronous ACP initialization.
Local terminal code also validates directories, handles buffer-name collisions, and cleans up newly created processes after display failures.
The [earlier review index](/Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/README.md) documents those fixes and their original evidence.
Those historical tests were not rerun for this comparison.

Some differences reflect stricter local choices.
Vegeta chooses the nearest Claude session and can fall back to an automatically preferred agent configuration.
The local code refuses ambiguous recovery and asks for a configuration when a recorded agent has no unique match.
The source establishes these differences, but the import commit gives no rationale for choosing the Vegeta behavior.

The next migration can use our standalone source as the implementation baseline under the `vegeta` namespace.
It needs a direct rename of definitions, feature names, properties, commands, and configuration references.
The existing [package declaration](/Users/ducnguyen/.emacs.d/lisp/package-declarations.el:1478) and [menu binding](/Users/ducnguyen/.emacs.d/lisp/package-declarations.el:500) still use `agent-sidebar`.
A reviewable port can separate shared browser fixes, Codex support, and command additions, with the corresponding tests in each change.
The dependency minimum needs a separate compatibility check before any reduction from `0.74.3`.
No source port, init edit, branch publication, or running-library replacement occurred during this investigation.

The [source snapshot](/Users/ducnguyen/.emacs.d/agent/vegeta-gap-2026-09-08/local-agent-sidebar.el) preserves the compared local file.
The [provenance file](/Users/ducnguyen/.emacs.d/agent/vegeta-gap-2026-09-08/provenance.json) records repository revisions and the runtime observation.

To replay the comparison, run:

```sh
python3 /Users/ducnguyen/.emacs.d/agent/vegeta-gap-2026-09-08/run.py
```

The runner uses the frozen source files and the installed Emacs 31 dependency directories.
It writes separate logs and reports each batch exit code.
Vegeta's 11 failed assertions are expected for the recorded commit.
