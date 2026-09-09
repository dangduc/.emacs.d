# Round 1: Dan Luu-inspired performance review

This is an analytical perspective inspired by Dan Luu's emphasis on measurements. It is not a review by Dan Luu.

Reviewed the immutable `baseline-0-agent-sidebar.el` and `baseline-1-agent-shell-sidebar.el` through `run-review.py`. The tests create synthetic files in temporary directories. They do not read private prompts or contact an agent service.

## Finding LUU-R1-1: P2 — Bound the preview before synchronous filtering

At frontend lines 616–618, the Codex parser retains the entire first prompt line. At lines 735–750, each filter builds and lowercases a string containing that preview. Lines 808–819 insert the entire preview into the sidebar. `agent-sidebar-set-filter` calls the shared synchronous redraw, so this work runs within a command without yielding.

A valid prompt containing a single 200,000-character line fits within the 262,144-byte read limit. With 300 such synthetic rollouts, the sidebar contains **60,007,270 characters**, despite showing only 300 chat rows. Filtering by an ordinary model name takes **601 ms**, compared with **32 ms** for the same 300 sessions with short prompts. The 30-session case already retains 6,000,787 characters and takes 76 ms to filter.

The positive control changes only the parser's returned preview to its first 200 characters. The same 300 files then produce a 67,270-character sidebar and an **18 ms** filter. Session IDs, model labels, paths, and row counts are unchanged. This isolates preview size as the source of the extra display and filter work.

This is a stress case involving many long first lines, not a claim that ordinary 300-session histories take 600 ms. It demonstrates an avoidable command stall for valid input. A single pasted minified payload can be long even when the sidebar is narrow and uses truncated display lines.

Suggested fix: give display previews a character bound before storing them in metadata. Keep the full transcript available through `o`. Document whether filtering searches only the bounded preview. If full-prompt filtering must remain supported, bound displayed text and move the large-text search out of synchronous redraw instead. Add a regression that checks bounded buffer size with the original files unchanged, and retain the long-line fixture as a control.

## Measurements without a blocking finding

| Fixture | Files | Maximum single parser call | Maximum parse/render callback | Filter | Sidebar characters |
| --- | ---: | ---: | ---: | ---: | ---: |
| Short prompt | 300 | 0.12 ms | 35 ms | 32 ms | 12,070 |
| 20,000-character session instructions plus 100,000-character injected context | 300 | 14.5 ms | 61 ms | 17 ms | 12,070 |
| Dense irrelevant event records within each prefix | 30 | 22 ms | 272 ms | 20 ms | 1,267 |
| 200,000-character first prompt line | 300 | 15.2 ms | 108 ms | 601 ms | 60,007,270 |
| Same long prompts, 200-character preview control | 300 | 15.5 ms | 110 ms | 18 ms | 67,270 |

The dense event fixture parses 119,040 JSON records, versus 90 for the corresponding short files, although all selected metadata appears in the first three records. This is an optimization opportunity at frontend lines 359–384 and 580–618. It is **not** evidence of a 272 ms keyboard stall: the shared parse loop checks `input-pending-p` between files, and the largest measured individual parse was 22 ms. The current bounded prefix keeps the cost of an individual local file below a long UI pause in these measurements.

The warm-refresh checks establish zero additional parser calls for unchanged files. I did not reproduce a cache-miss regression in these tests.

## Replay and scope

```sh
python3 /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/run-review.py \
  --probe /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-1/luu-tests.el \
  --selector '"^luu-"' \
  --log /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/round-1/luu.log
```

Result: **5/5 tests passed** on Emacs 31.1 in 6.45 seconds. The first three tests assert the current oversized-preview behavior as a reproduction; they are not tests that a fixed implementation should continue passing. The positive-control test verifies that limiting the returned preview changes the result. Timings are observations, not hard pass/fail thresholds. Runs use Emacs batch mode and manually dispatch idle callbacks, so they measure command and callback work, not GUI redisplay, operating-system scheduling, or remote filesystem latency.
