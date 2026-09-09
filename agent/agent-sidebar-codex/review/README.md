# Agent sidebar: two-round review

Completed two rounds with six subagent perspectives per round: John Ousterhout,
Dan Luu, Linus Torvalds, Kyle Kingsbury, an evidence skeptic, and a workflow
skeptic. These are analytical perspectives, not reviews by the named people.
Each report has executable evidence. Implementation work was delegated.

**18 distinct finding families addressed: 12 in Round 1 and 6 in Round 2.**
I1 and CW1 describe the same working-directory defect and are counted once.
All required rounds and fixes are complete.

The updated files are [the Desktop frontend](/Users/ducnguyen/Desktop/agent-sidebar.el)
and [its installed copy](/Users/ducnguyen/.emacs.d/lisp/agent-sidebar.el).
Only the frontend product source changed during this review. The shared core and
Emacs configuration still match their input hashes. The existing Codex test that
preferred a later event over the first prompt was corrected; [parser.md](fixes/parser.md)
explains that oracle change.

## Review reports

| Perspective | Round 1 | Round 2 |
| --- | --- | --- |
| Ousterhout | [Configuration ownership](round-1/ousterhout.md) | [Group membership](round-2/ousterhout.md) |
| Luu | [Preview costs and cache measurements](round-1/luu.md) | [Cold group validation](round-2/luu.md) |
| Torvalds | [Process identity and terminal names](round-1/torvalds.md) | [ACP boundary and display failures](round-2/torvalds.md) |
| Kingsbury | [Actions and active ownership](round-1/kingsbury.md) | [Ownership across buffer replacement](round-2/kingsbury.md) |
| Evidence skeptic | [Preview oracles and native record shapes](round-1/contrarian-evidence.md) | [Malformed schemas and cache boundaries](round-2/contrarian-evidence.md) |
| Workflow skeptic | [Incomplete rows and grouping](round-1/contrarian-workflow.md) | [Remote directories and command controls](round-2/contrarian-workflow.md) |

The reports distinguish real local subprocess checks, controlled reentrancy,
synthetic stress cases, and native record structure observations. Their original
failure logs remain intact. The probes did not send model prompts.

## Findings and delegated fixes

Every row below is resolved and covered by the final regression gate.

| ID | Priority | Reproduced defect | Fix evidence |
| --- | --- | --- | --- |
| O1 | P1 | ACP settings override the selected Codex home | [ACP configuration](fixes/acp.md) |
| O2 | P1 | Reload loses the selected home | [ACP configuration](fixes/acp.md) |
| O3 | P2 | A wrong-home candidate hides the matching buffer | [ACP configuration](fixes/acp.md) |
| R1-T1 | P2 | Equivalent home spellings create duplicate processes | [Terminal identity](fixes/terminal.md) |
| R1-T2 | P2 | `term` display-name collisions reuse the wrong child | [Terminal identity](fixes/terminal.md) |
| LUU-R1-1 | P2 | Long previews inflate synchronous display/filter work | [Bounded previews](fixes/performance.md) |
| K1 | P1 | A new ACP session's active native rollout can be deleted | [Native ownership](fixes/ownership.md) |
| K2 | P2 | Reentrant removal does not stop activation | [Command validation](fixes/workflow.md) |
| I1 / CW1 | P1 | `N` uses missing, ambient, or stale working-directory data | [Command validation](fixes/workflow.md) |
| CW2 | P2 | Grouping without package loses the intended provider | [Command validation](fixes/workflow.md) |
| CE-1 | P2 | Plugin inventories replace request previews | [Prompt selection](fixes/parser.md) |
| CE-2 | P2 | A later event replaces the first request | [Prompt selection](fixes/parser.md) |
| O4 | P2 | A stale repository group launches in another directory | [Group membership](fixes/group-membership.md) |
| R2-T1 | P2 | Display failures leave new terminal children untracked | [Display cleanup](fixes/display.md) |
| LUU-R2-1 | P2 | An already-ambiguous group still parses every entry | [Early ambiguity exit](fixes/group-membership.md) |
| K3 | P1 | Reload during a metadata read replaces the tracked owner | [Ownership transitions](fixes/ownership-transition.md) |
| CW3 | P2 | A local terminal action probes a remote directory first | [Local directory checks](fixes/local-cwd.md) |
| CE-3 | P2 | Unsupported JSON content shapes abort valid session parsing | [Schema guards](fixes/schema.md) |

## Final validation

| Check | Result | Evidence |
| --- | --- | --- |
| Emacs 31 source | 123/123 passed | [Log](final-source-tests.log) |
| Emacs 31 bytecode | 123/123 passed | [Log](final-compiled-tests.log) |
| Emacs 30 with source dependencies | 123/123 passed | [Log](final-emacs30-tests.log) |
| Compilation with warnings treated as errors | Passed | [Log](final-compile.log) |
| Shared core regressions | 131/131 passed; source unchanged | [Log](baseline-core-tests.log) |
| Four source mutation controls | All restored defects detected | [Context](fixes/final-parser-mutant-context.log), [chronology](fixes/final-parser-mutant-chronology.log), [Codex schema](fixes/final-schema-mutant-codex.log), [Claude schema](fixes/final-schema-mutant-claude.log) |
| Running Emacs 31.1, PID 3353 | Reviewed bytecode loaded; fixture passed | [Integration](live-check.eldata), [cleanup](live-final.eldata) |
| Desktop delivery | Frontend/core hashes match installed sources | [Hashes](desktop-sync.json) |

The live check uses a real idle timer and native ghostel with a local recorder.
It verifies rendering, arguments, working directory, home, process reuse, and
cleanup. The batch checks use the installed agent-shell ACP client with local
fixtures for load, new, reload, and ownership transitions. Provider authentication
and remote model responses were not exercised.

The 300-session long-preview stress case fell from 60,007,270 to 67,270 sidebar
characters. Its recorded filter measurement fell from 601 ms to 17.9 ms. The cold
ambiguous-group control reduced 300 metadata reads to two, from 2.297 seconds to
14 ms. These are isolated diagnostic observations, not GUI latency guarantees.
The regression gates check structural bounds instead of timing thresholds.

## Reproducibility

`inputs.json` and `baseline-*` preserve the first-round product input.
`round-2-inputs.json` and `round-2-agent-*.el` preserve the revised review input.
The absent-file ownership follow-up landed after that snapshot and is documented
in the ownership fix report. Frozen files were not overwritten.
`final-source-hashes.json`, `final-agent-*.el`, and `final-sidebar.patch` record
the delivered code. `final-manifest.json` records validation and artifact hashes.

Run from `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-sidebar-merge/run.py --probe agent/agent-sidebar-codex/review/regressions.el --log /private/tmp/sidebar-source-replay.log
python3 agent/agent-sidebar-merge/run.py --compiled --probe agent/agent-sidebar-codex/review/regressions.el --log /private/tmp/sidebar-bytecode-replay.log
python3 agent/agent-sidebar-merge/run.py --emacs /Applications/Emacs-30.app/Contents/MacOS/Emacs --source-dependencies --probe agent/agent-sidebar-codex/review/regressions.el --log /private/tmp/sidebar-emacs30-replay.log
```

For individual review probes, use `run-review.py` with the explicit frozen
`--front` and `--core` files named in that report. Without overrides, it uses the
Round 1 baseline. Save new replay logs separately from original evidence.

The combined loader registers shared fixture files once because Emacs 31 rejects
duplicate ERT definitions. Ordinary `load` is restored before tests execute.
Historical performance probes that assert the old oversized result are excluded;
their code and original logs remain available. Fixed-behavior tests check the
preview bound and early ambiguity rejection.

## Limits

Initial discovery and full redraw still scale with the number of entries.
Metadata outside the bounded prefix remains unavailable. Filtering searches only
the first 200 preview characters plus the existing metadata fields; `o` retains
access to the complete transcript. A reentrant refresh during activation requests
a retry instead of using an obsolete read.

Native ACP ownership needs a known UUID and home. The guard cannot identify every
external CLI process or a new CLI session whose UUID has not yet been associated
with a sidebar terminal. Existing path guards remain conservative for open
buffers. The review does not claim protection from every filesystem race.
