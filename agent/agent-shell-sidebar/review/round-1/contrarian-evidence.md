Round 1 found two parser failures in the frozen source. The existing 33 tests pass against that same source.
Five independent probes produced three passes and two failures. All fixtures used temporary directories in batch Emacs 31.1.
The runs used no user transcripts, running user Emacs, network requests, or external agents.

The source SHA256 is `ef3848084626c7ede22c2947bfdb16b1901e6201bab735bc96caba84356973a7`.
The [probe source](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/contrarian-evidence-probe.el) and [failure log](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/contrarian-evidence-probe.log) contain the fixtures and results.

**CE-1 — P2: The parser treats a partial metadata line as a complete session ID.**

Source: [baseline.el:272](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:272), with the read limit at line 260 and startup at line 824.
This contradicts the README claim that metadata beyond the read limit is unavailable.

The fixture places a session ID across byte 8192. Only the first four characters fit in the parser buffer.
The metadata regexp accepts the artificial buffer end as the line end.
The parser returns `"comp"`, and row activation passes `"comp"` to startup.
The stored ID is `"complete-session-identifier"`.
The expected result is an unavailable field or a parser error, without a request to resume the partial ID.

The log records:

```text
Cutoff: complete="complete-session-identifier" parsed="comp" start-session="comp" file-bytes=8220 visit-error=nil
```

The trigger requires a metadata line across the read limit. Ordinary short headers do not trigger this failure.
A separate control retains a complete ID whose terminating newline lands exactly at byte 8192.

Fix direction: If the read ends before the file ends, exclude any incomplete final metadata line.
Preserve complete lines at the limit. An explicit parser error can prevent accidental session creation or resumption.

**CE-2 — P2: Metadata trimming changes a valid recorded directory.**

Source: [baseline.el:273](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:273), with directory resolution at line 452.
This contradicts the README claim that resumption uses the recorded working directory.

The fixture creates two directories: `project` and `project `, where the second name ends in a space.
It uses the real `agent-shell--ensure-transcript-file` writer with `agent-shell-cwd-function` returning the second path without a final slash.
That directory path is valid under the documented callback contract.
The writer records the space. The sidebar removes it with `string-trim` and resolves the first directory.

The expected directory ends in `"/project /"`. The actual directory ends in `"/project/"`.
If the first directory does not exist, the same change causes a false missing-directory error.

Fix direction: Preserve trailing whitespace in the directory value.
Remove the metadata label and its formatting space.
Keep empty-field handling separate from path normalization.
The existing directory tests use paths with a final slash, which hides this failure.

**Mutation evidence for the current suite**

Two isolated mutations each pass all 33 existing tests:

| Mutation | Existing suite | Independent control |
| --- | --- | --- |
| Remove the 8192-byte read limit | 33/33 pass | Fails: reads the late `Agent` field |
| Remove the first-message boundary, retaining the separator | 33/33 pass | Fails: reads session and directory fields from prompt text |

The existing bounded-parser fixture places its late field after a separator.
That separator excludes the field even without a read limit.
The metadata-boundary fixture also includes a separator, so it does not exercise the first-message alternative.
These mutations demonstrate gaps in the assertions. They are not additional failures in the frozen implementation.

The independent controls pass against the frozen implementation and detect the corresponding mutations.
The [mutation generator](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/contrarian-evidence-mutations.py) creates separate source copies and leaves the baseline unchanged.
The `contrarian-evidence-suite-*.log` files record the three 33-test runs.
The `contrarian-evidence-controls-*.log` files record both rejected mutations.

The following commands reproduce the runs from any directory:

```sh
review=/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review
python3 "$review/run-probe.py" "$review/round-1/contrarian-evidence-probe.el" --ert --source "$review/baseline.el" --log "$review/round-1/contrarian-evidence-probe.log"
python3 "$review/round-1/contrarian-evidence-mutations.py"
python3 "$review/run-probe.py" "$review/round-1/contrarian-evidence-suite.el" --ert --source "$review/baseline.el" --log "$review/round-1/contrarian-evidence-suite-baseline.log"
for mutation in unbounded separator-only
do
  python3 "$review/run-probe.py" "$review/round-1/contrarian-evidence-suite.el" --ert --source "$review/round-1/contrarian-evidence-$mutation.el" --log "$review/round-1/contrarian-evidence-suite-$mutation.log"
  python3 "$review/run-probe.py" "$review/round-1/contrarian-evidence-controls.el" --source "$review/round-1/contrarian-evidence-$mutation.el" --log "$review/round-1/contrarian-evidence-controls-$mutation.log"
done
```

The main probe exits with status 1 for the two findings.
Each independent mutation control exits with status 1 for its intended failure.
All three existing-suite runs exit with status 0.
