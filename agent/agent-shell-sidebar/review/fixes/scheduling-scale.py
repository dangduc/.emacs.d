"""Compile scheduling copy and measure all timer callback work at three sizes."""
import argparse
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

here = Path(__file__).resolve().parent
root = here.parents[3]
parser = argparse.ArgumentParser()
parser.add_argument('--source', type=Path, default=here / 'scheduling.el')
parser.add_argument('--sizes', default='300,1200,3000')
parser.add_argument('--log', type=Path, default=here / 'scheduling-scale.log')
parser.add_argument('--window', action='store_true')
args = parser.parse_args()
outputs = []
with tempfile.TemporaryDirectory(prefix='sidebar-scheduling-compile-') as tmp:
    copy = Path(tmp) / 'agent-shell-sidebar.el'
    shutil.copyfile(args.source, copy)
    command = ['/Applications/Emacs.app/Contents/MacOS/Emacs', '-Q', '--batch', '-l', 'jka-compr']
    for name in ['shell-maker', 'acp', 'agent-shell']:
        command += ['-L', str(sorted((root / 'elpa/31').glob(name + '-[0-9]*'))[-1])]
    result = subprocess.run(command + ['--eval', '(setq byte-compile-error-on-warn t)', '-f', 'batch-byte-compile', str(copy)], capture_output=True, text=True, timeout=60)
    outputs.append('COMPILE\n' + result.stdout + result.stderr)
    if result.returncode:
        print(outputs[-1])
        raise SystemExit(result.returncode)
    for count in args.sizes.split(','):
        env = dict(os.environ, SCHEDULING_FILES=count)
        if args.window:
            env['SCHEDULING_WINDOW'] = '1'
        result = subprocess.run(command + ['-l', str(copy.with_suffix('.elc')), '-l', str(here / 'scheduling-scale.el')], env=env, capture_output=True, text=True, timeout=90)
        output = f'FILES {count}\n' + result.stdout + result.stderr
        outputs.append(output)
        print(output, flush=True)
        args.log.write_text('\n'.join(outputs))
        if result.returncode:
            raise SystemExit(result.returncode)
