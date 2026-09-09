`fzfa-rg` can leave Vertico empty until the producer finishes. The native `:stale` flag covers candidate pool growth as well as obsolete queries. Requiring every result to be complete and non-stale therefore withholds useful batches for the current query.

Accept nonempty batches as `partial` when the inspected, latest, and result request IDs match the owned request. Recheck snapshot identity and local handle/epoch ownership before publication. Retain the generation and presentation cache, and publish candidates with counts from the same snapshot. Empty partial results remain pending. Completed, non-stale zero-match results still clear the display.

The core completion paths and Helm adapter accept partial results. The change uses the existing native API and updates the architecture notes.

Validation:

- All 266 ERT tests pass with fzf-native 3.2 and with matching fzf-native 2.7 Lisp and module files.
- Byte compilation passes with warnings treated as errors.
- Five added tests cover streaming, cache reuse, request identity, snapshot races, ownership revocation, and empty results. The streaming assertions fail against unmodified fzfa.
- An isolated Emacs 31/Vertico probe with a two-million-line producer displayed no candidates before EOF on the original code. With this change, candidates appeared at 222 ms, before EOF at 572 ms.
- Isolated Emacs 31 sessions also displayed command candidates before EOF with Helm and Ivy, both alone and alongside a static source. The mixed-source probes observed `partial` output on screen while the producer ran.
- In Helm and Ivy, test keystrokes changed the query through `alpha`, `bravo`, a nonmatching query, `alpha`, and empty input. Matching lists updated, zero matches cleared the display, and empty input restored both groups. These checks used Helm 20260811.401 and Ivy 20260413.2102.
