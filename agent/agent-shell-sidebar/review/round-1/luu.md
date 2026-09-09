# Round 1: latency, scaling, and scheduling

This review uses a Dan Luu-inspired analytical lens. It does not represent Dan Luu or his conclusions.

The reviewed source is `lisp/agent-shell-sidebar.el`, with SHA256 `ef3848084626c7ede22c2947bfdb16b1901e6201bab735bc96caba84356973a7`.
The preserved source is [baseline.el](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el).
The review found two P2 defects.

## P2: full redraws defeat the parse chunk bound

Source: [agent-shell-sidebar.el:360](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:360).
The full row traversal is at [agent-shell-sidebar.el:711](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:711).

Trigger: open an expanded sidebar with 3,000 uncached transcripts and the default 30-file chunk size.
The expected behavior is bounded work between opportunities to process input.
The actual callback parses 30 files, then redraws every transcript without another input check.
The row count therefore scales with the total transcript count, despite the parse limit.

The compiled fixture produced these measurements:

| Transcripts | Initial refresh | Median callback | P95 callback | Maximum callback | Total callback work, including initial refresh | Transcript rows rendered |
| ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 300 | 39.5 ms | 11.9 ms | 25.8 ms | 25.8 ms | 0.173 s | 3,300 |
| 1,200 | 94.6 ms | 48.3 ms | 83.4 ms | 93.8 ms | 2.068 s | 49,200 |
| 3,000 | 240.6 ms | 119.6 ms | 203.1 ms | 223.6 ms | 12.464 s | 303,000 |

At 3,000 transcripts, header parsing took 0.179 seconds and redraws took 12.215 seconds.
The run allocated 21,574,913 cons cells and performed 29 garbage collections, which took 0.483 seconds.
The fixture counted one directory scan, 3,000 explicit attribute calls, and 3,000 header reads.
These counts do not include attribute operations inside the directory scan.

There are `ceil(N / 30)` parse callbacks and `1 + ceil(N / 30)` complete redraws.
The measured row counts equal `N * (1 + ceil(N / 30))`.
This establishes quadratic total rendering work for a cold metadata cache.
Warm refreshes at 3,000 transcripts took 58.0–77.8 milliseconds and read no headers.

The 12.464 seconds is cumulative synchronous work across 100 callbacks and the initial refresh.
It is not one continuous pause.
The callback measurements exclude timer delays and screen redisplay.
They include instrumentation overhead and describe this local fixture.

Recommended change: bound rendering work as well as header reads.
Update affected rows or use resumable rendering with a time budget and input checks.
Keep selection and grouping changes consistent across incremental updates.
A fixed 30-file parse limit alone cannot bound callback latency.

Reproduction command, from `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-shell-sidebar/review/round-1/luu-probe.py --compiled
```

The run exited with status 0.
The recorded measurements are in [luu-compiled.log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/luu-compiled.log).

## P2: timers retain the previous idle period's duration after input

Source: [agent-shell-sidebar.el:335](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:335).

Trigger: leave Emacs idle for five minutes, then type a command and stop typing.
The refresh interval remains 30 seconds.
The expected next refresh follows 30 seconds of the new idle period.
The actual pending timer requires 330 seconds of the new idle period.

The helper schedules an absolute idle threshold by adding the current idle duration to the configured delay.
That addition permits successive callbacks during continuous idle time.
Input resets Emacs idleness, but the sidebar does not reset the pending threshold.
The same helper controls parse callbacks, so interrupted parsing also retains its previous idle duration.

The probe used real Emacs timer objects and `timer-event-handler` to run ten refresh callbacks.
It supplied controlled `current-idle-time` values, then modeled input through an idle reset and `post-command-hook`.
The threshold sequence was `30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330` seconds.
After the modeled input, the timer threshold remained 330 seconds.
At 30 seconds of new idle time, the timer was ineligible to run.
Both source and compiled probes produced this result.

The installed `run-with-idle-timer` documentation defines its argument as the required duration of the next idle period.
The probe examines that stored threshold directly.
It does not simulate keyboard input in a running editor or wait 330 seconds.

Recommended change: reset pending timer thresholds when a new idle period begins.
Separate entry into an idle period from continuation within that period.
For example, use an idle-entry timer and short ordinary continuation timers that stop when input resumes.
Add a regression test for input between two parse callbacks and between two automatic refreshes.

Reproduction command:

```sh
python3 agent/agent-shell-sidebar/review/round-1/luu-probe.py --compiled --case lifecycle
```

The run exited with status 0.
The timer trace is in [luu-compiled-lifecycle.log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/luu-compiled-lifecycle.log).

## Scope and additional results

The probes ran in batch Emacs 31.1 on `aarch64-apple-darwin22.6.0` with installed package dependencies.
The compiled run used a temporary byte-compiled copy of the preserved source.
Every fixture used temporary local transcripts and deleted them after the run.
The probes did not use live Emacs, external services, or user transcripts.

“Cold” means an empty sidebar metadata cache.
The operating system file cache remained warm after fixture creation.
Each size ran once, with three warm refreshes afterward.
The bytecode build treated warnings as errors and passed.

A refresh after the first parse callback preserved the remaining 90-file queue in the 120-file fixture.
All 120 files required exactly one header read each.
Automatic refresh skipped the hidden buffer.
However, the hidden buffer retained its parse timer and parsed 30 files during a manually dispatched callback.
That callback rendered 120 rows and scheduled another parse timer.
This background work is additional evidence, not a separate finding in this review.

The source smoke run used this command and exited with status 0:

```sh
python3 agent/agent-shell-sidebar/review/round-1/luu-probe.py --sizes 300
```

Its log is [luu-source.log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/luu-source.log).
The probes are [luu-probe.el](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/luu-probe.el) and [luu-probe.py](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/luu-probe.py).
