# Emacs 30 migration & startup notes

Learnings from migrating this config from Emacs 29.3 → 30.2, sunsetting
straight.el, deferring startup, and building a native-comp Emacs. Practical
gotchas first; commands you can re-run are included.

## How to inspect a running Emacs from the shell

The fastest way to verify any of the below is to drive a live (or daemon) Emacs
with `emacsclient --eval`. `init.el` starts a server, and a daemon loads the full
config headlessly:

```bash
EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
EC=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
$EMACS --daemon=probe
$EC -s probe --eval '(emacs-init-time)'
$EC -s probe --eval '(length features)'
$EC -s probe --eval "(and (featurep 'org-roam) t)"
$EC -s probe --eval '(kill-emacs)'
```

For isolated checks use `--batch` (a fresh process that can't see your live
session): `$EMACS --batch -l early-init.el -l init.el`.

---

## 1. Major version migration (29 → 30): straight.el → package.el

straight.el was removed in favor of the built-in `package.el` + `use-package`
(both ship with Emacs 29+), plus `package-vc` for git-only packages.

- **Default install mechanism**: `init.el` sets `use-package-always-ensure t`, so
  a plain `(use-package foo)` installs `foo` from the archives in `early-init.el`.
- **Git-only packages → `:vc`**: `:vc (:url "https://…" :branch "…")`. Used for
  `seoul256-theme`, `vscode-icon`, `org-roam-ui`, `org-fc`, `mindre-theme`.
  `package-vc` does a plain `git clone`, which is actually a better fit than
  straight's `:files` for repos that ship build output or extra assets
  (`org-roam-ui/out`, `org-fc` awk scripts).
- **Built-ins / local packages opt out** with `:ensure nil` (`org`, `abbrev`,
  `tab-bar`, `whitespace`, `bind-key`, the local `duc` package, vendored
  `asy-mode`/`fruity-theme`, the `~/dev` rpgdm repos via `:load-path`). Without
  this, `always-ensure` tries to install them from an archive and fails
  ("package unavailable").

### Packages that needed special handling

- **Packages removed from MELPA** surface as "Package X is unavailable":
  `evil-ediff` (folded into `evil-collection` — just delete it and keep the plain
  `ediff` config; `evil-collection-init` supplies the keybindings) and
  `mindre-theme` (moved to `:vc` from its GitHub repo).
- **emacsql**: modern emacsql (GNU ELPA) has built-in SQLite support; the separate
  `emacsql-sqlite-builtin` package is folded in. Collapsed two
  `magit/emacsql :branch main` recipes into a single `(use-package emacsql)` and
  kept `(setq org-roam-database-connector 'sqlite-builtin)`.
- **Native modules**: `flx-rs` and `fuz` were dropped (need a Rust toolchain).
  `fzf-native` is **vendored** in `vendor/fzf-native/` (fork of
  `dangduc/fzf-native`) and kept — it ships prebuilt dynamic modules under
  `bin/`, so no compile step is needed (`fzf-native-load-dyn` selects
  `bin/Darwin/arm64/fzf-native-module.so` on Apple Silicon). `fussy` uses the
  native batch scorer via `(fussy-setup-fzf)`. Pure-elisp `flx` is kept only as
  a fallback scorer. If the prebuilt module ever fails to load on a new Emacs
  ABI, rebuild it with `M-x fzf-native-module-compile` (needs CMake).
- **asy-mode**: cloning the whole asymptote repo via `:vc` is wasteful (huge C++
  project). Vendored the single `base/asy-mode.el` into `vendor/asy-mode/` instead.
- **org-fc** (latest master): its algo classes inherit `eieio-singleton`, which
  lives in `eieio-base` — add `(require 'eieio-base)` in `:init` or it errors with
  "Given parent class eieio-singleton is not a class".

### Emacs 29/30 API deprecations fixed

- `focus-out-hook` (removed in 27) → `after-focus-change-function` +
  `(frame-focus-state)` check.
- `eval-after-load` (quoted-lambda form) → `with-eval-after-load`.
- `point-at-bol` / `point-at-eol` → `line-beginning-position` /
  `line-end-position`.
- Programmatic `goto-line` → `(goto-char (point-min))` + `forward-line`.
- `modus-themes-hl-line` face removed in modus-themes v4 → set the standard
  `hl-line` face. (A pre-existing missing-paren bug had masked this; fixing the
  paren exposed the dead-face error on `load-theme`.)

### First-run vs. persistent errors

On first boot, package install ordering produces transient errors that
**self-heal on the second run** (e.g. `compat-31` eager-macro-expansion,
org-roam "sqlitep nil"). Always run `--batch -l init.el` twice and only treat the
second run's errors as real. The remaining batch-only error
`duc/:config: Invalid font` is a headless artifact (`find-font` returns nil with
no GUI frame); it resolves in a real frame if the font is installed.

---

## 2. Startup deferral (init time 5.6s → ~2.6s)

Root cause: almost nothing was deferred. **A plain `(use-package foo)` with no
`:commands`/`:mode`/`:hook`/`:bind`/`:defer` eagerly `require`s the package at
startup.** Dozens of these meant ~everything loaded eagerly (912 features).

Fixes that moved the needle:

- Language modes → `:mode`; on-demand tools → `:commands`.
- **org-roam** was the single biggest cost (its `:config` ran
  `org-roam-db-autosync-mode`, forcing a DB sync). Load it on first command and
  move autosync to `(run-with-idle-timer 1 nil …)` so it happens just after init,
  off the critical path.
- **tree-sitter**: trigger `global-tree-sitter-mode` from `find-file-hook`, not
  `prog-mode-hook` — `*scratch*` is `lisp-interaction-mode`, which derives from
  `prog-mode`, so a `prog-mode-hook` trigger fires during startup.
- **The lambda-`:hook` gotcha**: `:hook (python-mode . (lambda () (require …)))`
  does **not** defer the package — use-package can't derive an autoload from a
  lambda, so with no other trigger it inserts an eager load-time `require`. Here
  that pulled all of `lsp-mode` in at startup. Fix: add `:defer t`. (Found by
  advising `require` to print a backtrace when the package loads.)

Measuring per-package cost: enable stats before the config loads (command-line
`--eval`/`-l` are processed in order):

```bash
$EMACS -Q --batch --eval '(setq use-package-compute-statistics t)' \
  -l early-init.el -l init.el \
  --eval '(let (r) (maphash (lambda (k v) (push (cons k (use-package-statistics-time v)) r)) use-package-statistics) (dolist (x (seq-take (sort r (lambda (a b) (> (cdr a) (cdr b)))) 25)) (princ (format "%6.3f  %s\n" (cdr x) (car x)))))'
```

Caveat: `use-package-statistics-time` measures wall-clock between a package's
init and config phases, so deferred packages look "expensive" (gap time, not
CPU). Cross-check with `(featurep 'pkg)` on a daemon to see what actually loaded
eagerly.

### Deferral exposed transitive-load regressions

Deferring packages removed transitive `require`s that other packages silently
relied on. Two surfaced as startup warnings:

- **`rpgdm-ironsworn`**: calls `f-join` at load time without `(require 'f)`; `f`
  used to be pulled in by an eagerly-loaded package. Fixed with `:init (require 'f)`.
- **`org-fc`** ("Symbol's value as variable is void: org-fc-algo-noop"): a deeper
  one, *not* really about deferral. `org-fc-algo-noop.el` is missing its
  `org-fc-core`/`eieio-base` requires (unlike `org-fc-algo-sm2.el`), so package.el
  byte/native-compiles it standalone with `eieio-singleton`/`org-fc-algo`
  undefined. The `defclass` then can't emit EIEIO's backward-compat
  class-name-as-variable binding, so the file's final
  `(org-fc-register-algo 'noop org-fc-algo-noop)` hits a void variable. The
  *source* loads fine — only the standalone-compiled file is broken. Durable
  config fix: in `:init`, `(require 'org-fc-core)` then force-load the algo from
  source so org-fc's own `require` skips the broken compiled file:
  `(let ((load-suffixes '(".el"))) (load "org-fc-algo-noop" nil t))`.

Method for diagnosing "void-variable/void-function at startup": reproduce in
`emacs -Q --batch` loading just the offending package; compare loading the raw
`.el` source vs the `.elc`; and advise `require` to print a backtrace to find who
pulls a package in (that's how the `lsp-python-ms` lambda-hook eager require was
found).

### Completion stack collapsed to vertico

Removed the dual ivy/counsel/swiper + vertico setup down to a single stack:
vertico + marginalia + orderless + `consult` (with `fussy`/`flx` matching).
Command remap reference:

| old (counsel/swiper) | new |
| --- | --- |
| `counsel-M-x` | built-in `execute-extended-command` (vertico UI) |
| `counsel-rg` | `consult-ripgrep` |
| `swiper` | `consult-line` |
| `counsel-apropos` | `apropos-command` |
| `counsel-descbinds` | `describe-bindings` |
| `counsel-fzf` | `project-find-file` / `consult-find` |
| `counsel-projectile-switch-project-action-*` | `projectile-switch-project` with `projectile-find-file` / `projectile-vc` / `consult-ripgrep` actions |

The bespoke `duc/ivy-*` helpers already used plain `completing-read`, so they
work unchanged under vertico (kept the legacy names). The one real rewrite was
the ripgrep→Org-link helper, which used ivy/swiper internals; reimplemented with
a `shell-command-to-string "rg …"` + `completing-read`.

---

## 3. native-comp build from source (macOS / Apple Silicon)

Built a native-comp Emacs 30.2 from `emacs-mirror` because the installed
`Emacs.app` lacked native compilation (`system-configuration-features` had no
`NATIVE_COMP`).

```bash
# Deps (Homebrew). libgccjit is the native-comp backend.
brew install libgccjit gcc texinfo jansson    # NOT tree-sitter — see below

cd ~/src && git clone --depth 1 --branch emacs-30 git@github.com:emacs-mirror/emacs.git
cd emacs

export PATH="/opt/homebrew/opt/texinfo/bin:$PATH"   # for makeinfo
export LIBRARY_PATH="/opt/homebrew/opt/gcc/lib/gcc/current:/opt/homebrew/opt/libgccjit/lib/gcc/current:$LIBRARY_PATH"

./autogen.sh
./configure --with-native-compilation=aot --without-tree-sitter \
            --with-modules --with-ns          # self-contained NS app (default)
make -j$(sysctl -n hw.ncpu)
make install                                   # builds nextstep/Emacs.app

ditto nextstep/Emacs.app /Applications/Emacs.app
```

### Build gotchas

- **tree-sitter version mismatch (the build-breaker)**: Homebrew's
  `tree-sitter 0.26.x` removed `ts_language_version`, which Emacs 30's
  `treesit.c` still calls — `make` fails with "call to undeclared function
  'ts_language_version'". Since this config uses the **ELPA** `tree-sitter`
  package (its own dynamic module, independent of built-in `treesit`), the fix is
  `--without-tree-sitter`. (Alternative: install tree-sitter 0.24.x.)
- **`--with-json` is obsolete** in Emacs 30 (JSON/jansson auto-detected) — a
  harmless "unrecognized option" warning.
- **Self-contained vs `--disable-ns-self-contained`**: for a drop-in
  `/Applications/Emacs.app`, build self-contained (the default; do NOT pass
  `--disable-ns-self-contained`). That bundles lisp + AOT `.eln` inside the
  `.app` so it needs no installed prefix tree, and `make install` writes into
  `nextstep/Emacs.app` without sudo. With `--disable-ns-self-contained`,
  `make install` tries to write to `/usr/local` (needs sudo) and the app warns it
  can't find its lisp dir.
- **Where the AOT `.eln` land**: `Contents/Frameworks/native-lisp/` inside the
  bundle (~1600 files). Your ELPA packages native-compile lazily into
  `~/.emacs.d/eln-cache/` as you load them.
- **Verify**: `Emacs -Q --batch --eval '(princ (native-comp-available-p))'` → `t`.
- Locally-built app has no Gatekeeper quarantine (no `xattr` cleanup needed).
  Old apps preserved: `Emacs-nonc.app` (non-native 30.2), `Emacs-old.app` (29.3).

### Key learning: native-comp does NOT speed up startup

Warm-cache startup was ~2.8s native-comp vs ~2.6s non-native — same, marginally
slower (`.eln` files are larger to *load* than `.elc`). **Startup is load/eval
bound; native-comp accelerates CPU-bound runtime execution** (redisplay/font-lock
on large files, completion filtering over big candidate sets, lsp, org/org-roam
operations), not init time. The startup reduction (5.6s → 2.6s) came entirely
from deferral, not from native-comp.
