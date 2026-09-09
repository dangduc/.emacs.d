"""Measure sidebar callbacks using temporary local transcripts and batch Emacs."""
import argparse
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

here = Path(__file__).resolve().parent
review = here.parent
root = review.parents[2]
parser = argparse.ArgumentParser()
parser.add_argument('--source', type=Path, default=review / 'baseline.el')
parser.add_argument('--compiled', action='store_true')
parser.add_argument('--sizes', default='300,1200,3000')
parser.add_argument('--case', choices=['all', 'scale', 'lifecycle'], default='all')
args = parser.parse_args()
outputs = []

with tempfile.TemporaryDirectory(prefix='sidebar-luu-compile-', dir='/private/tmp') as temp:
    source = args.source.resolve()
    if args.compiled:
        source = Path(temp) / 'agent-shell-sidebar.el'
        shutil.copyfile(args.source, source)
        command = ['/Applications/Emacs.app/Contents/MacOS/Emacs', '-Q', '--batch', '-l', 'jka-compr']
        for name in ['shell-maker', 'acp', 'agent-shell']:
            command += ['-L', str(sorted((root / 'elpa/31').glob(name + '-[0-9]*'))[-1])]
        command += ['--eval', '(setq byte-compile-error-on-warn t)', '-f', 'batch-byte-compile', str(source)]
        result = subprocess.run(command, capture_output=True, text=True, timeout=60)
        outputs.append('COMPILE ' + result.stdout + result.stderr)
        if result.returncode:
            print(outputs[-1])
            raise SystemExit(result.returncode)
        source = source.with_suffix('.elc')
    cases = [('scale', value) for value in args.sizes.split(',')] if args.case != 'lifecycle' else []
    if args.case != 'scale':
        cases.append(('lifecycle', '120'))
    for case, count in cases:
        env = dict(os.environ, LUU_PROBE_CASE=case, LUU_PROBE_FILES=count)
        command = [sys.executable, str(review / 'run-probe.py'), str(here / 'luu-probe.el'), '--source', str(source)]
        result = subprocess.run(command, capture_output=True, text=True, timeout=105, env=env)
        output = f'CASE {case} files={count}\n' + result.stdout + result.stderr
        outputs.append(output)
        print(output, flush=True)
        label = 'compiled' if args.compiled else 'source'
        suffix = '' if args.case == 'all' else '-' + args.case
        (here / f'luu-{label}{suffix}.log').write_text('\n'.join(outputs))
        if result.returncode:
            raise SystemExit(result.returncode)
