"""Compile the frozen round-two source and run independent local fixtures."""
import argparse
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

here = Path(__file__).resolve().parent
root = here.parents[3]
parser = argparse.ArgumentParser()
parser.add_argument('--case', choices=['all', 'long-preview', 'unique-agents', 'names', 'idle-reset'], default='all')
parser.add_argument('--files', default='3000')
args = parser.parse_args()
outputs = []
with tempfile.TemporaryDirectory(prefix='sidebar-luu-r2-', dir='/private/tmp') as directory:
    source = Path(directory) / 'agent-shell-sidebar.el'
    shutil.copyfile(here / 'reviewed.el', source)
    cmd = ['/Applications/Emacs.app/Contents/MacOS/Emacs', '-Q', '--batch', '-l', 'jka-compr']
    for name in ['shell-maker', 'acp', 'agent-shell']:
        cmd += ['-L', str(sorted((root / 'elpa/31').glob(name + '-[0-9]*'))[-1])]
    result = subprocess.run(cmd + ['--eval', '(setq byte-compile-error-on-warn t)', '-f', 'batch-byte-compile', str(source)], capture_output=True, text=True, timeout=60)
    outputs.append('COMPILE\n' + result.stdout + result.stderr)
    if result.returncode:
        print(outputs[-1])
        raise SystemExit(result.returncode)
    cases = ['long-preview', 'unique-agents', 'names', 'idle-reset'] if args.case == 'all' else [args.case]
    for case in cases:
        env = dict(os.environ, LUU_CASE=case, LUU_FILES=args.files)
        result = subprocess.run(cmd + ['-l', str(source.with_suffix('.elc')), '-l', str(here / 'luu-probe.el')], capture_output=True, text=True, timeout=90, env=env)
        output = f'CASE {case}\n' + result.stdout + result.stderr
        outputs.append(output)
        print(output, flush=True)
        suffix = '' if args.case == 'all' else '-' + args.case
        (here / f'luu-probe{suffix}.log').write_text('\n'.join(outputs))
        if result.returncode:
            raise SystemExit(result.returncode)
