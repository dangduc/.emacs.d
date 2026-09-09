"""Run an isolated ERT probe against an explicit sidebar source pair."""
import argparse
import pathlib
import shutil
import subprocess
import tempfile

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parents[2]
p = argparse.ArgumentParser()
p.add_argument("--front", type=pathlib.Path, default=HERE / "baseline-0-agent-sidebar.el")
p.add_argument("--core", type=pathlib.Path, default=HERE / "baseline-1-agent-shell-sidebar.el")
p.add_argument("--probe", type=pathlib.Path, required=True)
p.add_argument("--log", type=pathlib.Path, required=True)
p.add_argument("--selector", default="t")
p.add_argument("--emacs", default="/Applications/Emacs.app/Contents/MacOS/Emacs")
p.add_argument("--source-dependencies", action="store_true")
a = p.parse_args()
with tempfile.TemporaryDirectory(prefix="sidebar-review-", dir="/private/tmp") as temporary:
    directory = pathlib.Path(temporary)
    shutil.copy2(a.core, directory / "agent-shell-sidebar.el")
    shutil.copy2(a.front, directory / "agent-sidebar.el")
    command = [a.emacs, "-Q", "--batch", "-l", "jka-compr", "-L", str(directory)]
    for package in ["shell-maker", "acp", "agent-shell"]:
        dependency = sorted((ROOT / "elpa/31").glob(package + "-[0-9]*"))[-1]
        if a.source_dependencies:
            target = directory / dependency.name
            shutil.copytree(dependency, target, ignore=shutil.ignore_patterns("*.elc", "*.eln"))
            dependency = target
        command += ["-L", str(dependency)]
    command += ["-l", str(directory / "agent-shell-sidebar.el"), "-l", str(directory / "agent-sidebar.el"),
                "-l", str(a.probe.resolve()), "--eval", "(ert-run-tests-batch-and-exit '" + a.selector + ")"]
    result = subprocess.run(command, capture_output=True, text=True, timeout=120)
a.log.parent.mkdir(parents=True, exist_ok=True)
a.log.write_text(result.stdout + result.stderr)
print(result.stdout + result.stderr)
raise SystemExit(result.returncode)
