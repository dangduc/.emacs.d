This checkpoint captures the non-ignored changes before Emacs configuration surgery.
It includes unrelated configuration changes, investigation artifacts, logs, caches, archives, and local build outputs.
Existing ignore rules still exclude package installations, private state, and other ignored files.

The parent repository remains on `migrate-emacs30`.
The fzf-native and fzfa changes have separate local commits on `codex/checkpoint-before-emacs-surgery-20260908` in each submodule.
Their previous branches retain their original commits.
The parent commit records the new submodule commits.
No commits were pushed.

The existing `fruity-theme`, `melpa`, and Vegeta comparison repositories are now registered submodules.
Their source files and checked-out commits remain unchanged.
Their Git metadata now resides inside the parent `.git` directory.
The fzfa metadata also moved there.

The fzf-native repository retains its existing metadata location because another linked worktree uses it.
Git does not support automatic metadata relocation for that layout.
A verified recovery bundle also preserves its checkpoint branch and reachable history:

`/Users/ducnguyen/.emacs.d/.git/checkpoints/fzf-native-before-emacs-surgery-20260908.bundle`

The [manifest](/Users/ducnguyen/.emacs.d/agent/pre-surgery-checkpoint-20260908/manifest.json) records the original and checkpoint commits, branches, repository locations, and bundle path.
This Git checkpoint does not contain ignored working files or every file in the Emacs directory.
