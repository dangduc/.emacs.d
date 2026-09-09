# K3 fix: resolve current owners after reading

Changed only `agent-sidebar--codex-owned-file-p` in `lisp/agent-sidebar.el`.

Before the metadata read, the guard now retains matching Codex homes rather than candidate buffer objects. After the selected file's bounded read, it enumerates current buffers and compares their current UUID and home. A session reloaded into a replacement buffer therefore remains an owner. A replacement with another UUID or home does not.

The guard still reads only the selected file once. It does not scan the transcript store. The earlier absent-file correction remains in place.

Thirteen focused tests pass against the current source. These include the actual ACP reload invoked from `after-insert-file-functions`, the native-guard mutation control, the original new-session deletion reproduction, reload without path markers, cross-home/config/UUID controls, malformed and absent files, and a new replacement-buffer matrix. All three matrix cases perform exactly one metadata read.

Observed after the fix:

```text
RELOAD DURING READ: old-live=nil new-live=t file-exists=t retained-mark=delete
Replacement matrix: same-session ownership=t reads=1
Replacement matrix: other-session ownership=nil reads=1
Replacement matrix: other-home ownership=nil reads=1
```

Replay:

```sh
python3 /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/run-review.py \
  --front /Users/ducnguyen/.emacs.d/lisp/agent-sidebar.el \
  --core /Users/ducnguyen/.emacs.d/lisp/agent-shell-sidebar.el \
  --probe /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/fixes/ownership-transition-tests.el \
  --log /Users/ducnguyen/.emacs.d/agent/agent-sidebar-codex/review/fixes/ownership-transition.log \
  --selector '(or "^ownership-" "^kingsbury-r2-")'
```

`ownership-transition-tests.el` loads the earlier probes and adds the replacement matrix. `ownership-transition.log` contains all thirteen passing results. The immutable Round 2 report and failing log remain unchanged. No real model prompt, Desktop synchronization, running Emacs reload, or byte compilation was performed by this fix agent.
