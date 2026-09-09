"""Bound FIFO reads in an isolated batch Emacs, using synthetic data only."""
from pathlib import Path
import json
import os
import subprocess
import sys
import tempfile
import time

root = Path(__file__).resolve().parents[4]
source = Path(sys.argv[1]).resolve()
command = ['/Applications/Emacs.app/Contents/MacOS/Emacs', '-Q', '--batch']
for name in ['shell-maker', 'acp', 'agent-shell']:
    command += ['-L', str(sorted((root / 'elpa/31').glob(name + '-[0-9]*'))[-1])]
command += ['-l', str(source)]
with tempfile.TemporaryDirectory(prefix='sidebar-workflow-fifo-') as directory:
    transcript_directory = Path(directory) / '.agent-shell/transcripts'
    transcript_directory.mkdir(parents=True)
    fifo = transcript_directory / 'pending.md'
    os.mkfifo(fifo)
    expression = '''(let* ((file %s)
                         (root %s)
                         (agent-shell-sidebar--file-info (make-hash-table :test 'equal)))
      (message "FIFO type=%%S modes=%%S regular=%%S discovered=%%S"
               (file-attribute-type (file-attributes file))
               (file-attribute-modes (file-attributes file))
               (file-regular-p file)
               (member file (agent-shell-sidebar--transcripts-for-root root)))
      (message "FIFO header=%%S" (agent-shell-sidebar--ensure-parsed file)))''' % (
        json.dumps(str(fifo)), json.dumps(directory + '/'))
    started = time.monotonic()
    try:
        result = subprocess.run(command + ['--eval', expression], capture_output=True,
                                text=True, timeout=3)
        payload = {'timed_out': False, 'returncode': result.returncode,
                   'output': result.stdout + result.stderr}
    except subprocess.TimeoutExpired as error:
        def decode(value):
            return value.decode() if isinstance(value, bytes) else (value or '')
        payload = {'timed_out': True, 'output': decode(error.stdout) + decode(error.stderr)}
    payload['elapsed_seconds'] = round(time.monotonic() - started, 3)
    print(json.dumps(payload))
