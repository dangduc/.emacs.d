# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

This is a personal Emacs configuration (`user-emacs-directory`). The "build" is Emacs itself loading the config; there is no compiled artifact or test suite.

## Common commands

- `make` — runs `git submodule update --init --recursive` and sets `diff.ignoreSubmodules dirty`. Run this after cloning or when `vendor/` submodules are missing.
- The configured Emacs binary is `/Applications/Emacs.app/Contents/MacOS/Emacs` (macOS).
- To smoke-test a config change without touching the running editor: `/Applications/Emacs.app/Contents/MacOS/Emacs -Q --batch -l early-init.el -l init.el` (note `-Q` skips the normal init, so this only verifies that the files load).
- Byte-compilation is automatic: `auto-compile` (set up in `early-init.el`) recompiles `.el` files on load and on save. There is no separate compile step to run.

## Load order

Startup follows a deliberate sequence — changes that depend on earlier setup must respect it:

1. **`early-init.el`** — loaded by Emacs 27+ automatically (and manually by `init.el` on Emacs 26-). Sets up `auto-compile` (from `vendor/`), `package-archives`/priorities, and `package-quickstart`. Marked `no-byte-compile`.
2. **`init.el`** — the entry point. In order: GC/UI tuning → `local-declarations.el` then optional `local.el` (machine-specific, gitignored) → adds every subdir of `vendor/` to `load-path` → bootstraps **straight.el** → `(require 'package-declarations)` → a large `custom-set-variables` block.
3. **`lisp/package-declarations.el`** — the bulk of the config. One big sequence of `use-package` forms (package install + configuration + keybindings). This is where most edits go.
4. **`lisp/duc.el`** — provides the `duc` feature, loaded via `(use-package duc :straight nil ...)` from `package-declarations.el`. Holds all personal `duc/`-prefixed commands (font sizing, eshell/shell helpers, eval-dwim functions, theme helpers, buffer utilities).

## Package management

Two coexisting mechanisms — know which applies before adding a package:

- **straight.el** is the default (`straight-use-package-by-default t`). New packages are declared with `use-package` in `lisp/package-declarations.el` and are fetched/built into `straight/` (gitignored). No `:straight t` needed.
- **git submodules** in `vendor/` (see `.gitmodules`: `auto-compile`, `packed`, `pulsar`). Used for packages needed before straight bootstraps, or pinned manually. Every `vendor/` subdir is on `load-path`. Use `:straight nil` in `use-package` to reference a `vendor/` or local package (e.g. the `duc` package).
- The legacy `package.el` archives are still configured in `early-init.el` and the `package-selected-packages` list in `init.el`'s `custom-set-variables` is mostly stale theme cruft — prefer straight/`use-package` for anything new.

## Conventions

- Personal commands and vars are namespaced `duc/...` and live in `lisp/duc.el`.
- Keybindings are defined inside the relevant `use-package` block (the config uses `evil`, `general`, and `hydra`).
- `local.el` is the place for machine-specific, non-committed settings; it is loaded if present and is gitignored.
- All elisp files use lexical binding (`-*- lexical-binding: t; -*-`).
