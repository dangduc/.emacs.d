⏺ Audit complete. I ran the suite, byte-compiled, and traced all 33 commits.

  Topology

  dangduc/fzfa is 33 ahead, 0 behind; merge-base is upstream's tip (9927468). A PR would
  apply cleanly — the problem is not conflicts, it's the shape of the history.
  
  Why so many commits
  
  30 content commits + 3 merges, by subject prefix:
  
  ┌──────────────────────┬─────┬────────────────────────────────────────────────────────┐
  │        Prefix        │  n  │                 What they actually are                 │
  ├──────────────────────┼─────┼────────────────────────────────────────────────────────┤
  │ fix:                 │ 17  │ Fixes to code introduced in this same branch, not      │
  │                      │     │ upstream bugs                                          │
  ├──────────────────────┼─────┼────────────────────────────────────────────────────────┤
  │ ci:                  │ 6   │ Six bumps of one pinned SHA                            │
  ├──────────────────────┼─────┼────────────────────────────────────────────────────────┤
  │ feat:                │ 1   │ The actual feature (5f9f593, +1198/−220)               │
  ├──────────────────────┼─────┼────────────────────────────────────────────────────────┤
  │ WIP:                 │ 1   │ fd5d1ca — unfinished-by-name                           │
  ├──────────────────────┼─────┼────────────────────────────────────────────────────────┤
  │ release:/docs:/test: │ 3   │                                                        │
  ├──────────────────────┼─────┼────────────────────────────────────────────────────────┤
  │ (unprefixed)         │ 2   │ The two original commits                               │
  └──────────────────────┴─────┴────────────────────────────────────────────────────────┘
  
  So it's one feature plus 17 self-corrections and 6 pin bumps — review-response churn
  preserved verbatim. Four of the "fixes" are larger than the feature they fix:
  
  16f4d72 fix: harden interactive frontend ownership      +1538/-363
  8e7f689 fix: harden reentrant frontend publications     +1454/-407
  b4c4f56 fix: close reviewed interactive frontend gaps   +1213/-512
  4a70a57 fix: bind async publication to frontend owners  +1011/-151
  5f9f593 feat: integrate persistent fzf-native sessions  +1198/-220   <-- the feature
  
  The frontend was rewritten roughly four times in-branch to land a net +9104/−1504.
  
  Two concrete history defects:
  
  - Duplicate commit. 9cbbadb and 025dec7 have identical subjects and byte-identical
  content, but different parents — 9cbbadb was cut from upstream 9927468 for PR #3 while
  025dec7 sat on the session branch off 5f9f593. Git merged them without conflict. The
  final tree is correct (no duplicate defs, byte-compile clean), but the change appears
  twice in the log.
  - No-op merge. 79b3a5f merges 13b73c6 + 8e0e99a, but 8e0e99a already contains 13b73c6.
  
  What the commits do
  
  Net +9104/−1504 over 8 files; 72 new top-level definitions, 5 removed, and zero new 
  defcustoms — no new user-facing configuration. Four separable concerns:
  
  ┌───────────────────┬──────┬─────────────────────────────────────────────────────────┐
  │      Concern      │ New  │                         Effect                          │
  │                   │ defs │                                                         │
  ├───────────────────┼──────┼─────────────────────────────────────────────────────────┤
  │                   │      │ Persistent fzf-native session replaces per-keystroke    │
  │ Session API       │ 11   │ batch re-scoring: submit → poll status → materialize    │
  │ integration       │      │ one snapshot; producer (find/fd/rg) stays alive for     │
  │                   │      │ incremental retries                                     │
  ├───────────────────┼──────┼─────────────────────────────────────────────────────────┤
  │ Frontend          │      │ The churn magnet — stops stale async callbacks          │
  │ ownership / epoch │ 11   │ publishing into a minibuffer or helm buffer that has    │
  │  fencing          │      │ moved on                                                │
  ├───────────────────┼──────┼─────────────────────────────────────────────────────────┤
  │ Water-fill        │      │ fzfa-max-candidates becomes one session-wide budget,    │
  │ presentation      │ 9    │ fair-shared across multi-sources, with completed sparse │
  │ budget            │      │  sources donating slots                                 │
  ├───────────────────┼──────┼─────────────────────────────────────────────────────────┤
  │ Producer          │      │ Classify producer by arity instead of firing it as a    │
  │ lifecycle / arity │ 7    │ construction probe; invalidate deferred refreshes on    │
  │                   │      │ cleanup                                                 │
  └───────────────────┴──────┴─────────────────────────────────────────────────────────┘
  
  Also: fzfa-test.el 2804 → 8123 lines (221 → 376 tests), architecture.org +31, README.org
  +117, new benchmarks/presentation-budget.el.                                        
  
  The code holds up. make test → 376/376, 0 unexpected. Byte-compile is clean. No dead code
  — every new definition is referenced beyond its own defun.
  
  Recommended refactors before PR
  
  1. Repoint the CI pin — blocker. .github/workflows/test.yaml checks out repository: 
  fastducduc/fzf-native at ref: df22813… in two steps. That makes upstream's CI depend on a
  personal scratch fork, and a SHA pin there can become unreachable if the branch is
  deleted or force-pushed and GitHub GCs the object — that fork is actively rewritten (I
  saw cf16fea…8355574 force-update this session). The SHA is absent from
  jojojames/fzf-native.
  
  Fix: point at the canonical dangduc/fzf-native at tag 2.7. I verified df22813 is an
  ancestor of 2.7, 2.7 is on dangduc's main, and 2.7 carries the session ABI. This deletes
  all 6 ci: commits.
  
  2. Restructure into a stacked series — blocker. Squash 30 commits into four PRs in
  dependency order, mirroring the concern split:
  
  1. Producer arity classification + cleanup invalidation (no session dependency —
  smallest, lands first)
  2. Session API integration + ABI handshake
  3. Frontend ownership / epoch fencing
  4. Water-fill presentation budget + benchmark
  
  Drop the WIP: commit, the duplicate, and the no-op merge. If you'd rather ship one PR,
  squash to 4–6 commits at minimum — 17 fix: commits correcting unreleased code is noise a
  reviewer has to read past.
  
  3. Add a fallback CI lane. The workflow comments "Do not silently exercise the legacy
  fallback against fzf-native's default branch." That's a deliberate choice, but it means
  CI never runs against a stock fzf-native. In tests, fzfa--session-api-p is stubbed t 18
  times and nil only 4. Upstream fzfa users on an older fzf-native will hit exactly that
  path, so this is the first thing jojojames will ask about. Add a lane pinned to a
  pre-session tag (e.g. 2.2) running the same suite.
  
  4. Split the file. fzfa.el goes 5147 → 6869 lines. The ownership/epoch group (11 defs)
  and the water-fill group (9 defs) are cohesive and self-contained; extracting
  fzfa-ownership.el and fzfa-budget.el would make the diff reviewable and matches the
  existing one-concern-per-file layout.
  
  5. Nit: (defvar helm-pattern) appears twice in fzfa-helm.el, lines 233 and 248.
  
  One caveat on scope: I audited the fork against upstream and ran its own suite. I did not
  run fzfa against a pre-session fzf-native, so the fallback path's actual health is
  unverified — that's the gap item 3 is meant to close.
