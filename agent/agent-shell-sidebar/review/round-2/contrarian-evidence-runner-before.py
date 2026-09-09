"""Run baseline tests, delegated regressions, and independent parser controls."""
from pathlib import Path
import argparse
import subprocess
import tempfile

here = Path(__file__).resolve().parent
root = here.parents[2]
parser = argparse.ArgumentParser()
parser.add_argument('--source', type=Path, default=root / 'lisp/agent-shell-sidebar.el')
parser.add_argument('--log', type=Path, default=here / 'regressions.log')
parser.add_argument('--emacs', default='/Applications/Emacs.app/Contents/MacOS/Emacs')
parser.add_argument('--source-dependencies', action='store_true')
args = parser.parse_args()
files = [here.parent / 'tests/sidebar-tests.el']
files += [here / 'fixes' / (name + '-tests.el') for name in ['identity', 'metadata', 'scheduling']]
files += [here / 'round-1' / name for name in [
    'ousterhout-probe.el', 'torvalds-probe.el', 'kingsbury-probe.el',
    'contrarian-evidence-probe.el', 'contrarian-workflow.el']]
for file in files:
    if not file.exists():
        raise SystemExit(f'Missing completed regression file: {file}')
with tempfile.TemporaryDirectory(prefix='sidebar-regression-', dir='/private/tmp') as temp:
    loader = Path(temp) / 'load-tests.el'
    loader.write_text(';;; -*- lexical-binding: t; -*-\n' + '\n'.join(f'(load "{file}" nil t)' for file in files) + '\n')
    command = ['python3', str(here / 'run-probe.py'), str(loader), '--source', str(args.source),
               '--ert', '--log', str(args.log), '--emacs', args.emacs]
    if args.source_dependencies:
        command.append('--source-dependencies')
    result = subprocess.run(command, capture_output=True, text=True, timeout=105)
print(result.stdout + result.stderr)
raise SystemExit(result.returncode)
