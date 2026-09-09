# Local terminal working-directory validation

Implemented workflow finding CW3 in the shared terminal resume helper and the
initial terminal launch guard.

Resume now rejects non-string, relative, remote, or missing CWDs before constructing
the reuse identity. Remote paths are rejected before filesystem existence checks.
The direct launch guard also rejects remote paths before calling `file-directory-p`.
Local session arguments, identities, and reuse behavior remain covered by tests.

## Verification

- `local-cwd-tests.log`: 7/7 pass. The Round 2 RET/A/N fixture records zero remote
  directory probes for all three commands. A direct-launch probe records none.
  Invalid CWD values are rejected before expansion or directory checks.
- `local-cwd-existing-tests.log`: existing 31/31 tests pass, including Codex resume
  argv/home/reuse, Claude terminal argv/reuse, actual local term subprocesses,
  ghostel failure cleanup, and dedicated-sidebar/vterm behavior.

The invalid-CWD test initially counted a native-compilation trampoline's temporary
file check as an application probe. That output is retained in
`local-cwd-initial-probe-noise.log`. The corrected recorder only counts operations
on the tested CWD and delegates unrelated operations. The final run passes.

The original Round 2 review log remains unchanged. Remote file handlers are disabled
in the rejection fixtures, so no network connection or authentication was attempted.

## Replay

From `/Users/ducnguyen/.emacs.d`:

```sh
python3 agent/agent-sidebar-codex/review/run-review.py \
  --front lisp/agent-sidebar.el --core lisp/agent-shell-sidebar.el \
  --probe agent/agent-sidebar-codex/review/fixes/local-cwd-tests.el \
  --selector '(or "^fix-local-cwd-" "^workflow-r2-")' \
  --log agent/agent-sidebar-codex/review/fixes/local-cwd-replay.log
```

No running Emacs, Desktop copy, or compiled library was changed by this assignment.
