# Round 2: latency, scaling, and scheduling

This review uses a Dan Luu-inspired analytical lens. It does not represent Dan Luu or his conclusions.

The reviewed source is [reviewed.el](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/reviewed.el).
Its SHA256 is `629a0b61dc25a1b9766936b34b655aea9e1775c57f0f2d98ebb9ef00fea1b6ed`.
The probes found no additional defects in the measured workloads.

## L1: parse callbacks now include bounded staging

The earlier implementation redrew every transcript after each 30-file parse chunk.
The revised [parse callback](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/reviewed.el:425) advances a staged snapshot.
The [render loop](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/reviewed.el:995) permits at most 300 path or row operations per callback.
The [publication function](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/reviewed.el:956) still copies the completed buffer synchronously.

The measurements dispatched real timer objects through `timer-event-handler` until both parsing and staging finished.
An attached batch window exercised publication and location restoration.
The following measurements include native publication:

| Transcripts | Initial synchronous refresh | Callbacks | Median callback | P95 callback | Maximum callback | Cumulative work | Maximum publication |
| ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 300 | 26.8 ms | 12 | 3.3 ms | 27.0 ms | 27.0 ms | 155.7 ms | 0.6 ms |
| 1,200 | 102.9 ms | 54 | 5.7 ms | 27.2 ms | 27.5 ms | 812.6 ms | 17.9 ms |
| 3,000 | 226.9 ms | 126 | 7.9 ms | 24.0 ms | 42.8 ms | 1,844.1 ms | 25.7 ms |

At 3,000 transcripts, six snapshots required 21,000 row renders, including the initial view.
Each callback parsed at most 30 files and rendered at most 300 transcript rows.
Four snapshots displayed parsed metadata before the parse queue emptied.
The final assertions required no parse queue, timer, render iterator, dirty flag, or render error.
They also required the final transcript prompt to appear.

These results support the L1 correction for the measured workloads.
They do not establish a general callback deadline.
Initial refresh and publication still contain work proportional to the view size.

Reproduction command, from `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-shell-sidebar/review/fixes/scheduling-scale.py --source agent/agent-shell-sidebar/review/round-2/reviewed.el --sizes 300,1200,3000 --window --log agent/agent-shell-sidebar/review/round-2/luu-scale.log
```

The command exited with status 0.
The complete measurements are in [luu-scale.log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/luu-scale.log).

## Independent workload shapes

The new probes used 3,000 transcripts with two additional shapes:

| Shape | Initial refresh | Callbacks after parsing finished | Median callback | Maximum callback | Maximum publication | Cumulative work | Warm refresh range |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 7,000-character preview per transcript | 225.5 ms | 26 | 24.0 ms | 89.1 ms | 41.4 ms | 3,680.8 ms | 651.4–683.0 ms |
| Distinct agent name per transcript | 229.1 ms | 59 | 2.5 ms | 39.1 ms | 8.5 ms | 1,220.7 ms | 54.8–75.9 ms |

The long-preview view contained 21,099,057 characters after the final publication.
The log field `buffer-bytes` records `buffer-size`, which counts characters despite that field name.
Visible prompt counts progressed through `30, 720, 1410, 2130, 2820, 3000`.
Publications occurred at callbacks `21, 42, 63, 84, 105, 126`.
Parsing finished at callback 100, so stopping at an empty parse queue omits 26 callbacks.

The distinct-agent fixture also completed all metadata and staging work.
It required 159 callbacks, including 59 callbacks after parsing finished.
Visible prompt counts progressed through `30, 720, 1471, 2310, 3000, 3000`.

The independent callback measurements include a prompt-count observation after each publication.
Native publication measurements exclude that observation.
The reported callback maxima therefore include observer overhead.
All final-state assertions passed, and warm refreshes required no header reads or subsequent parse timer.

The long-preview warm refresh remains a synchronous cost of about 0.65–0.68 seconds in this fixture.
The relevant path is [the complete redraw](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/reviewed.el:811).
This report records that limitation without classifying every linear operation as a defect.

A separate computation used colliding project basenames without filesystem operations.
Disambiguation took 1.0 ms for 300 roots, 7.1 ms for 1,200 roots, and 33.1 ms for 3,000 roots.
The probe required unique names and the expected parent suffix.
These measurements do not include discovery of those projects.

## L2: completed commands reset the pending thresholds

The [reset function](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/reviewed.el:461) cancels pending timers and schedules their base delays.
The mode registers that function through [the global post-command hook](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/reviewed.el:1051).

The independent probe first supplied 300 seconds of current idle time.
Its real parse and refresh timers contained thresholds of 300.1 and 330 seconds.
The probe modeled a completed command from another buffer through `post-command-hook`.
The new thresholds were 0.1 and 30 seconds.
The old timer objects were absent from `timer-idle-list`.

This result supports the L2 correction for completed commands.
It does not cover an unfinished key prefix, raw keyboard input, or every possible command-loop interruption.
The Kingsbury perspective investigates unfinished prefixes separately.

Reproduction command:

```sh
python3 agent/agent-shell-sidebar/review/round-2/luu-probe.py
```

The command exited with status 0 for both workload shapes, name disambiguation, and the completed-command reset.
Its evidence is in [luu-probe.log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/luu-probe.log).
The new probe sources are [luu-probe.el](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/luu-probe.el) and [luu-probe.py](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-2/luu-probe.py).

## Measurement limits

The probes ran on batch Emacs 31.1 with temporary bytecode and local fixtures.
Byte compilation treated warnings as errors and passed.
The probes did not modify the frozen source or contact a live Emacs process or external service.

Cumulative work is the sum of initial refresh and callback durations.
It excludes idle delays and screen redisplay.
It is not elapsed time until the final metadata appears.
At the default delay, each successive callback requires another 0.1 seconds of idle time.
The callback counts therefore matter to user-visible completion, even after synchronous callback work decreases.

The workload fixtures used an empty metadata cache and recently created files.
The operating system file cache remained warm.
Each shape ran once, followed by three warm refreshes.
The round-one and round-two fixtures differ in metadata and grouping, so their timing ratios are not controlled benchmark comparisons.
