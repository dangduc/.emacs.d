# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

This is a personal Emacs configuration (`user-emacs-directory`). The "build" is Emacs itself loading the config; there is no compiled artifact or test suite.

## Common commands

- `make` — runs `git submodule update --init --recursive` and sets `diff.ignoreSubmodules dirty`. Run this after cloning or when `vendor/` submodules are missing.
- The configured Emacs binary is `/Applications/Emacs.app/Contents/MacOS/Emacs` (macOS) — GNU Emacs 31.1, built from the release tarball at `~/src/emacs-31.1` with `--with-native-compilation=aot --with-tree-sitter --with-modules --with-ns`. The previous 30.2.50 build stays at `/Applications/Emacs-30.app` as a rollback, and the config still starts cleanly under it.
- To smoke-test a config change without touching the running editor: `/Applications/Emacs.app/Contents/MacOS/Emacs -Q --batch -l early-init.el -l init.el` (note `-Q` skips the normal init, so this only verifies that the files load).
- Installing a new build: `cp -Rp` the bundle from `~/src/emacs-<version>/nextstep/Emacs.app` into `/Applications`. Plain `cp -R` stamps every file with the copy time, and with `load-prefer-newer t` that makes Emacs prefer the gzipped `jka-compr.el.gz` over its `.elc` and die at startup with "Recursive load" (`early-init.el` requires `jka-compr` up front to guard against this).
- Byte-compilation is automatic: `auto-compile` (set up in `early-init.el`) recompiles `.el` files on load and on save. There is no separate compile step to run.

## Load order

Startup follows a deliberate sequence — changes that depend on earlier setup must respect it:

1. **`early-init.el`** — loaded by Emacs 27+ automatically (and manually by `init.el` on Emacs 26-). Sets up `auto-compile` (from `vendor/`), `package-archives`/priorities, and `package-quickstart`. Marked `no-byte-compile`.
2. **`init.el`** — the entry point. In order: GC/UI tuning → `local-declarations.el` then optional `local.el` (machine-specific, gitignored) → `package-initialize` → adds every subdir of `vendor/` to `load-path` (after `package-initialize`, so vendored forks shadow same-named ELPA packages) → `(require 'package-declarations)` → a large `custom-set-variables` block.
3. **`lisp/package-declarations.el`** — the bulk of the config. One big sequence of `use-package` forms (package install + configuration + keybindings). This is where most edits go.
4. **`lisp/duc.el`** — provides the `duc` feature, loaded via `(use-package duc :ensure nil ...)` from `package-declarations.el`. Holds all personal `duc/`-prefixed commands (font sizing, eshell/shell helpers, eval-dwim functions, theme helpers, buffer utilities).

## Package management

Targets Emacs 30+; the installed build is 31.1 (`/Applications/Emacs.app`). Three mechanisms — know which applies before adding a package:

- **Built-in `package.el` + `use-package`** is the default. `init.el` sets `use-package-always-ensure t`, so a plain `(use-package foo ...)` installs `foo` from the archives declared in `early-init.el` (MELPA/melpa-stable/GNU/Org). No `:ensure t` needed.
- **`package-vc` via `:vc`** for packages only available as a git repo (not on an archive). Use the use-package `:vc` keyword, e.g. `:vc (:url "https://github.com/owner/repo" :branch "main")`. Current `:vc` packages: `seoul256-theme`, `vscode-icon`, `org-roam-ui`, `org-fc`, `fussy`.
- **Built-ins and local packages** opt out with `:ensure nil` (e.g. `org`, `abbrev`, `tab-bar`, `whitespace`, `bind-key`, the local `duc` package in `lisp/`, the vendored `asy-mode`/`fruity-theme`, and the `~/dev` rpgdm repos loaded via `:load-path`).
- **git submodules** in `vendor/` (see `.gitmodules`: `auto-compile`, `packed`, `pulsar`) are loaded before `package.el` initializes (`auto-compile` in `early-init.el`). Every `vendor/` subdir is added to `load-path` in `init.el`.

Two Emacs versions share this directory, so everything version-sensitive is keyed by `emacs-major-version` in `early-init.el`: packages in `elpa/<major>/`, the activation file as `package-quickstart-<major>.el`, and native code in `eln-cache/<version>-<abi-hash>/`. The eln cache is keyed by build ABI as well as version, so reconfiguring the *same* version (adding `--with-tree-sitter`, say) starts a fresh cache and orphans the old directory.

straight.el was removed in the Emacs 30 migration; there is no `straight/` bootstrap and no `:straight` keys. See `docs/emacs-30-migration.md` for the migration, startup-deferral, and native-comp build notes. The `package-selected-packages` list in `init.el`'s `custom-set-variables` is stale theme cruft, not the source of truth — `lisp/package-declarations.el` is.

## Conventions

- Personal commands and vars are namespaced `duc/...` and live in `lisp/duc.el`. (A few keep legacy `duc/ivy-*` names but use plain `completing-read`.)
- Keybindings are defined inside the relevant `use-package` block (the config uses `evil`, `general`, and `hydra`).
- **Completion:** vertico + marginalia + orderless + `consult` (with `fussy`/`flx` matching). The old ivy/counsel/swiper stack was removed; use `consult-*` and built-ins (`project-find-file`, `apropos-command`, …) for new bindings.
- **Startup/deferral:** prefer lazy loading — give each `use-package` a `:commands`/`:mode`/`:hook`/`:bind` trigger (or `:defer t`) so it stays off the startup path. Note: a `:hook` whose function is a *lambda* does not defer on its own — add `:defer t`. Both installed builds are native-compiled AOT, so `native-comp-available-p` is `t`. Daemon startup with this config measures ~2.4s on 31.1 against ~3.3s on 30.2.50.
- **Tree-sitter:** built-in `treesit`, turned on config-wide with `treesit-enabled-modes t`, which copies `treesit-major-mode-remap-alist` into `major-mode-remap-alist` so each language opens in its `FOO-ts-mode`. Grammars live in `~/.emacs.d/tree-sitter/` (gitignored); `treesit-auto-install-grammar` defaults to `ask`. When adding a mode hook, note the ts modes derive from `FOO-base-mode`, not `FOO-mode` — hook `python-base-mode`, `js-base-mode`, `sh-base-mode` or `typescript-ts-base-mode`, and name both modes where there is no shared base (`java-mode` *and* `java-ts-mode`). The third-party `tree-sitter` / `tree-sitter-langs` packages were dropped.
- `local.el` is the place for machine-specific, non-committed settings; it is loaded if present and is gitignored.
- All elisp files use lexical binding (`-*- lexical-binding: t; -*-`).
