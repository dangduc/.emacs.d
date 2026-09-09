# Round 1: practical correctness and API contracts

Reviewed the sidebar source, its README and 33-test suite, and the installed agent-shell 0.74.3 implementation. The startup keywords and buffer return value match this installed version. Two actionable findings follow.

The reviewed source and `review/baseline.el` both have SHA256 `ef3848084626c7ede22c2947bfdb16b1901e6201bab735bc96caba84356973a7`.

**T1 — P2: Compare file identity before deleting an owned transcript.**

Location: [agent-shell-sidebar.el:912](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:912), specifically the path-string comparison at line 914 and deletion at line 917.

Trigger: A known project directory is a symbolic-link alias of the directory used by an open agent-shell. Discovery accepts its regular transcript files. Marking the alias row and confirming deletion then compares the alias spelling with the shell's original spelling.

Expected: Refuse deletion of the owned transcript and retain its mark, as the command's documented contract requires. Actual: The same file is deleted and the mark is removed. The fixture uses the real `agent-shell-buffers` implementation with a temporary owner buffer; no agent process runs. A control with identical path spellings preserves the file.

Recorded output:

```text
CONTROL: exact-owner-path owned-file-exists=t retained-mark=delete
ALIAS: discovered=t same-file=t exact-owner-path-match=nil owner-listed=t
Deleted 1 transcript(s)
DELETION: owned-file-exists=nil retained-mark=nil
```

Fix direction: Use filesystem identity, such as `file-equal-p`, for existing owned files. Guard absent owner paths and file errors. Cover both directory aliases and identical paths. Comparing normalized strings alone still misses directory symlinks.

**T2 — P2: Retry cached read failures when refreshing or activating a row.**

Location: [agent-shell-sidebar.el:311](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/baseline.el:311), especially the unconditional cached-header return at line 314. Lines 292–309 define the signature and accept error headers as cache hits; lines 326–329 consequently exclude them from the parse queue.

Trigger: A transcript is unreadable during parsing. Its read permission is restored without changing its content. The modification time, size, and inode remain unchanged.

Expected: `g` or `RET` retries the now-readable transcript. Actual: Refresh queues zero files, and activation's `--ensure-parsed` still returns the old permission error. The independent direct parser reads the restored file successfully. The global cache also survives sidebar buffer recreation.

Recorded output, with the temporary filename omitted:

```text
READABILITY: before=unreadable after=readable direct-parser-session="fixture-session" signature-unchanged=t
RECOVERY: pending-after-refresh=0 cached-error-after-retry="Opening input file: Permission denied, ..."
```

Fix direction: Distinguish failed reads from successful cached metadata. Explicit refresh and activation should retry failures; automatic retry should remain bounded by the existing scheduler.

Run both reproductions and the control from `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-shell-sidebar/review/run-probe.py agent/agent-shell-sidebar/review/round-1/torvalds-probe.el --source agent/agent-shell-sidebar/review/baseline.el --ert --log agent/agent-shell-sidebar/review/round-1/torvalds-probe.log
```

[Probe source](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/torvalds-probe.el) and [full output](/Users/ducnguyen/.emacs.d/agent/agent-shell-sidebar/review/round-1/torvalds-probe.log) are saved alongside this review. Emacs 31.1 and agent-shell 0.74.3 ran three tests: the control passed, and both expected-behavior assertions failed. Exit status: 1. All files, modes, marks, and buffers belong to temporary fixtures and are cleaned up.
