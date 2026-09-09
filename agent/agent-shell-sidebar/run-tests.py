"""Run sidebar tests with installed agent-shell and no user init."""
import argparse
from pathlib import Path
import subprocess
import shutil
import tempfile

root = Path(__file__).resolve().parents[2]
parser = argparse.ArgumentParser()
parser.add_argument('--original', action='store_true')
parser.add_argument('--emacs', default='/Applications/Emacs.app/Contents/MacOS/Emacs')
parser.add_argument('--compile', action='store_true')
parser.add_argument('--compiled', action='store_true')
parser.add_argument('--source-dependencies', action='store_true')
args = parser.parse_args()
cmd = [args.emacs, '-Q', '--batch', '-l', 'jka-compr']
dependency_temp = tempfile.TemporaryDirectory(prefix='sidebar-deps-', dir='/private/tmp') if args.source_dependencies else None
for name in ['shell-maker', 'acp', 'agent-shell']:
    directory = sorted((root / 'elpa/31').glob(name + '-[0-9]*'))[-1]
    if dependency_temp:
        destination = Path(dependency_temp.name) / directory.name
        shutil.copytree(directory, destination, ignore=shutil.ignore_patterns('*.elc', '*.eln'))
        directory = destination
    cmd += ['-L', str(directory)]
source = root / ('agent/agent-shell-sidebar/original.el' if args.original else 'lisp/agent-shell-sidebar.el')
if args.compile:
    cmd += ['--eval', '(setq byte-compile-error-on-warn t)', '-f', 'batch-byte-compile', str(source)]
else:
    if args.compiled:
        source = source.with_suffix('.elc')
    cmd += ['-l', str(source), '-l', str(Path(__file__).parent / 'tests/sidebar-tests.el'),
            '--eval', "(ert-run-tests-batch-and-exit '(tag regression))" if args.original
            else '(ert-run-tests-batch-and-exit t)']
try:
    r = subprocess.run(cmd, capture_output=True, text=True, timeout=90)
finally:
    if dependency_temp:
        dependency_temp.cleanup()
log = Path(__file__).parent / ('compile.log' if args.compile else 'original-tests.log' if args.original
                             else 'compiled-tests.log' if args.compiled else
                             'emacs30-tests.log' if 'Emacs-30' in args.emacs else 'tests.log')
log.write_text(r.stdout + r.stderr)
print(r.stdout + r.stderr)
raise SystemExit(r.returncode)
