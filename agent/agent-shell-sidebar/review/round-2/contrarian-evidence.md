# Round 2: contrarian review of the test evidence

No new product defect was established. The combined suite detects both assertion gaps from Round 1 and the two parser regressions. A test-runner isolation fault was reproduced and fixed within the authorized scope.

Reviewed source: `review/round-2/reviewed.el`.
SHA256: `629a0b61dc25a1b9766936b34b655aea9e1775c57f0f2d98ebb9ef00fea1b6ed`.
Environment: batch Emacs 31.1, installed agent-shell 0.74.3, local temporary fixtures.

## Runner isolation: reproduced, corrected, and controlled

The initial combined run passed 93 of 94 tests. Only `identity-ambiguous-alias-obeys-explicit-selection-on-every-visit` failed, with an attempted minibuffer read and `end-of-file`.

`round-1/contrarian-workflow.el:5` sets `native-comp-enable-subr-trampolines` to nil while loading. The installed `agent-shell-select-config` function is native compiled. With trampolines disabled, that dependency bypasses the test's `completing-read` replacement.

A two-process control isolates the cause without changing the identity test:

| Loading policy | Before / after workflow load | Native selector | Unchanged identity test |
| --- | --- | --- | --- |
| Original global assignment | t / nil | Yes | Fails: reads stdin |
| Dynamic binding around load | t / t | Yes | Passes |

The authorized edit to `review/run-regressions.py` dynamically preserves that variable around each test-file load. It preserves the incoming policy; it does not force native compilation on or change test assertions. The original reviewer file remains intact.

The corrected combined runner passes **94/94 tests**. The default-policy control still fails when isolation is removed, so this result does not come from suppressing the failed test.

Evidence: `contrarian-evidence-before-runner.log`, `contrarian-evidence-after-runner.log`, and the `contrarian-evidence-policy-{leaked,isolated}.{el,log}` files. `contrarian-evidence-policy-results.json` records both exact control commands. `contrarian-evidence-runner-before.py` preserves the prior runner; `contrarian-evidence-runner.py` freezes the corrected 94-test runner before subsequent coordinator additions.

This is an integration fault in the review harness, not a new sidebar defect. The change was limited to the authorized runner and this review's evidence files.

## Causal parser verification

I generated four isolated mutations from the frozen Round 2 source. None survives the combined suite.

| Runtime | Combined suite | Fresh independent controls |
| --- | ---: | ---: |
| Reviewed source | 94/94 pass | 4/4 pass |
| Remove the read limit | 89/94 pass; five intended failures | 2/4 pass |
| Remove the first-message boundary | 93/94 pass; boundary control fails | 3/4 pass |
| Accept an incomplete final metadata line | 91/94 pass; three intended failures | 3/4 pass |
| Trim recorded directory whitespace | 91/94 pass; three intended failures | 3/4 pass |

The unbounded and separator-only variants each passed all 33 original tests in Round 1. They now fail the independent controls included in the combined runner. The byte-limit control contains no earlier separator that could mask an unbounded read. The first-message control does not depend on a preceding separator.

The incomplete-line mutation retains the parser's error reporting but accepts fields at the artificial buffer end. The new metadata regressions still detect it. Their assertions inspect the parsed fields rather than merely accepting any error marker.

The whitespace mutation restores trimming for CWD while leaving other parser logic intact. The real-writer regressions detect the resulting directory change.

Four fresh tests in `contrarian-evidence-probe.el` provide independent controls:

- A 24-case matrix crosses byte lengths 8191, 8192, and 8193 with ASCII or UTF-8 padding, newline termination, and true EOF or additional file content. It distinguishes complete lines from cut fields and preserves a complete unterminated field at true EOF.
- Three boundary fixtures use a separator, a User heading, or an Agent heading before metadata-shaped message content. Only the earlier metadata remains available.
- A long file with no boundary excludes a late session ID while retaining its earlier Agent field.
- The installed transcript writer records a directory ending in a tab and space. The parser and directory resolver retain that exact path while a distinct trimmed directory also exists.

All four pass on the reviewed source. Each mutant fails its relevant control. These tests use real local files and the installed transcript writer; they do not stub parser results or edit cached headers.

## Prior findings

- **CE-1:** corrected. The byte-cutoff matrix rejects partial IDs, and complete fields at the boundary or true EOF remain available. Restoring partial-line acceptance fails the combined suite.
- **CE-2:** corrected. The writer-to-parser-to-directory path preserves whitespace. Restoring trimming fails both existing regressions and the fresh compound-whitespace control.
- **Read-limit assertion gap:** closed in the combined runner. The corresponding mutation is rejected.
- **First-message assertion gap:** closed in the combined runner. The corresponding mutation is rejected independently of the separator.

The identity and scheduling regressions also pass in the 94-test integration run. This perspective did not repeat their performance measurements or independently claim every concurrency schedule from their reports.

## Reproduction and evidence limits

Run the complete frozen mutation experiment from any directory:

```sh
python3 /Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/contrarian-evidence-mutations.py
```

The script uses the preserved 94-test runner. `--controls-only` reruns the four independent controls against all five runtime variants. The original combined commands used `review/run-regressions.py` before the coordinator's later additions; those exact commands remain recorded in `contrarian-evidence-mutation-results.json`.

The `contrarian-evidence-combined-*.log` and `contrarian-evidence-controls-*.log` files hold raw ERT output. Each mutant source is retained. `contrarian-evidence-provenance.json` records hashes of the reviewed source, evidence code, preserved runner, and relevant test files. The independent probe logs its loaded sidebar source before and after execution.

During probe development, I corrected an unmatched parenthesis, a nil argument to `insert`, and an invalid fixture location outside the transcript tree. These were test-code faults, not sidebar findings. `contrarian-evidence-probe-development.json` preserves the initial run summaries. The table above uses the corrected control runs only; the combined 94-test runs were unaffected.

No installed product source, original reviewer assertion, user transcript, running Emacs process, external service, or git state changed. The suite's existing ACP fixture uses a local mock process. This evidence covers local batch behavior on this Mac; it does not establish atomic filesystem guarantees or results on other Emacs versions.
