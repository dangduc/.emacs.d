#!/usr/bin/env python3
"""Create independent vendor sources for one Emacs major version.

Usage: python3 scripts/copy-vendor.py 30
Existing destinations are never overwritten. Git packages retain their history
and remote URLs. Sources and native modules are copied; old Lisp bytecode and
build directories are not copied. Run this explicitly, never during startup.
"""

import argparse
import json
import shutil
import subprocess
import tempfile
from pathlib import Path


def git(directory, *args):
    return subprocess.run(
        ["git", "-C", str(directory), *args],
        check=True, capture_output=True, text=True,
    ).stdout.strip()


def excluded(_directory, names):
    return [name for name in names if name in {
        ".git", ".DS_Store", "build", "dist", ".eask", "__pycache__",
    } or name.endswith((".elc", ".eln", ".pyc"))]


def finish_git_copy(source, target, major):
    """Preserve tracked sources and commit the exclusion of compiled Lisp."""
    tracked = subprocess.run(
        ["git", "-C", str(source), "ls-files", "-z"],
        check=True, capture_output=True, text=True,
    ).stdout.split("\0")
    bytecode = []
    for relative in filter(None, tracked):
        src, dst = source / relative, target / relative
        if relative.endswith((".elc", ".eln")):
            bytecode.append(relative)
            if dst.exists() or dst.is_symlink():
                dst.unlink()
            continue
        if src.exists() or src.is_symlink():
            dst.parent.mkdir(parents=True, exist_ok=True)
            if dst.is_symlink() or (src.is_symlink() and dst.exists()):
                dst.unlink()
            shutil.copy2(src, dst, follow_symlinks=False)
        elif dst.exists() or dst.is_symlink():
            dst.unlink()
    ignore = target / ".gitignore"
    content = ignore.read_text() if ignore.exists() else ""
    for pattern in ("*.elc", "*.eln"):
        if pattern not in content.splitlines():
            content = content.rstrip("\n") + "\n" + pattern + "\n"
    ignore.write_text(content)
    git(target, "add", "--", ".gitignore", *bytecode)
    changed = subprocess.run(
        ["git", "-C", str(target), "diff", "--cached", "--quiet"], check=False,
    ).returncode
    if changed:
        git(target, "commit", "-m", f"Keep Emacs {major} compiled Lisp out of source snapshots")
    return git(target, "rev-parse", "HEAD")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("major", type=int)
    parser.add_argument("--source", type=Path)
    args = parser.parse_args()
    if args.major < 29:
        parser.error("This configuration requires Emacs 29 or newer")
    root = Path(__file__).resolve().parent.parent
    source = (args.source or root / "vendor").resolve()
    destination = root / f"vendor-{args.major}"
    if destination.exists() or destination.is_symlink():
        parser.error(f"Destination already exists: {destination}")
    if not source.is_dir():
        parser.error(f"Source directory does not exist: {source}")

    staging = Path(tempfile.mkdtemp(prefix=f".vendor-{args.major}-", dir=root))
    manifest = {"major": args.major, "source": str(source), "packages": []}
    try:
        for package in sorted(source.iterdir()):
            if package.name.startswith("."):
                continue
            target = staging / package.name
            record = {"name": package.name}
            if package.is_dir() and (package / ".git").exists():
                head = git(package, "rev-parse", "HEAD")
                # Independent object storage: no alternates or hard links.
                subprocess.run(
                    ["git", "clone", "--quiet", "--no-hardlinks", "--no-checkout",
                     str(package), str(target)], check=True,
                )
                git(target, "branch", f"codex/emacs-{args.major}", head)
                git(target, "symbolic-ref", "HEAD", f"refs/heads/codex/emacs-{args.major}")
                git(target, "reset", "--mixed", head)
                git(target, "remote", "remove", "origin")
                for remote in git(package, "remote").splitlines():
                    urls = git(package, "remote", "get-url", "--all", remote).splitlines()
                    git(target, "remote", "add", remote, urls[0])
                    for url in urls[1:]:
                        git(target, "remote", "set-url", "--add", remote, url)
                    # Explicit push URLs can differ from fetch URLs.
                    explicit_push = subprocess.run(
                        ["git", "-C", str(package), "config", "--get-all",
                         f"remote.{remote}.pushurl"], capture_output=True, text=True,
                    )
                    for url in explicit_push.stdout.splitlines():
                        git(target, "remote", "set-url", "--add", "--push", remote, url)
                record["commit"] = head
                record["source_status"] = git(package, "status", "--short")
            if package.is_dir():
                shutil.copytree(package, target, dirs_exist_ok=True,
                                symlinks=True, ignore=excluded)
                # Do not carry compiled Lisp even if a repository tracks it.
                for pattern in ("*.elc", "*.eln"):
                    for compiled in target.rglob(pattern):
                        if ".git" not in compiled.relative_to(target).parts:
                            compiled.unlink()
                if (package / ".git").exists():
                    record["snapshot_commit"] = finish_git_copy(package, target, args.major)
            elif package.name not in excluded(source, [package.name]):
                shutil.copy2(package, target)
            manifest["packages"].append(record)
        for link in staging.rglob("*"):
            if link.is_symlink() and not link.resolve().is_relative_to(staging):
                raise RuntimeError(f"Source link escapes the new vendor directory: {link}")
        (staging / ".source-snapshot.json").write_text(json.dumps(manifest, indent=2) + "\n")
        # Atomic publication after every package copy succeeds.
        staging.rename(destination)
        print(json.dumps({"directory": str(destination), **manifest}, indent=2))
    finally:
        if staging.exists():
            shutil.rmtree(staging)


if __name__ == "__main__":
    main()
