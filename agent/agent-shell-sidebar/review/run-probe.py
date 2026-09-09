"""Run a review probe in batch Emacs with installed dependency paths."""
from pathlib import Path
import argparse
import subprocess
import shutil
import tempfile

root = Path(__file__).resolve().parents[3]
parser = argparse.ArgumentParser()
parser.add_argument('probe', type=Path)
parser.add_argument('--source', type=Path, default=root / 'lisp/agent-shell-sidebar.el')
parser.add_argument('--log', type=Path)
parser.add_argument('--ert', action='store_true')
parser.add_argument('--emacs', default='/Applications/Emacs.app/Contents/MacOS/Emacs')
parser.add_argument('--source-dependencies', action='store_true')
args = parser.parse_args()
command = [args.emacs, '-Q', '--batch', '-l', 'jka-compr']
dependencies = tempfile.TemporaryDirectory(prefix='sidebar-probe-deps-', dir='/private/tmp') if args.source_dependencies else None
for name in ['shell-maker', 'acp', 'agent-shell']:
    directory = sorted((root / 'elpa/31').glob(name + '-[0-9]*'))[-1]
    if dependencies:
        destination = Path(dependencies.name) / directory.name
        shutil.copytree(directory, destination, ignore=shutil.ignore_patterns('*.elc', '*.eln'))
        directory = destination
    command += ['-L', str(directory)]
command += ['-l', str(args.source.resolve()), '-l', str(args.probe.resolve())]
if args.ert:
    command += ['-f', 'ert-run-tests-batch-and-exit']
try:
    result = subprocess.run(command, capture_output=True, text=True, timeout=90)
finally:
    if dependencies:
        dependencies.cleanup()
output = result.stdout + result.stderr
if args.log:
    args.log.write_text(output)
print(output)
raise SystemExit(result.returncode)
