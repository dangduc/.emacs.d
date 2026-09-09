"""Independent changed-file View command checks, with bounded child processes."""
from pathlib import Path
import hashlib
import json
import os
import subprocess
import sys
import tempfile
import time

root = Path(__file__).resolve().parents[4]
source = Path(sys.argv[1]).resolve()
command = ['/Applications/Emacs.app/Contents/MacOS/Emacs', '-Q', '--batch', '-l', 'jka-compr']
for name in ['shell-maker', 'acp', 'agent-shell']:
    command += ['-L', str(sorted((root / 'elpa/31').glob(name + '-[0-9]*'))[-1])]
command += ['-l', str(source)]
results = []
for scenario in ['regular', 'rewritten-regular', 'missing', 'directory', 'symlink', 'fifo']:
    with tempfile.TemporaryDirectory(prefix='sidebar-cw2-view-', dir='/private/tmp') as directory:
        folder = Path(directory) / '.agent-shell/transcripts'
        folder.mkdir(parents=True)
        file = folder / 'fixture.md'
        file.write_text('**Agent:** Fixture\n---\noriginal contents\n')
        expression = r'''(progn
          (require 'cl-lib)
          (let* ((file %s) (root %s) (scenario %s)
                 (agent-shell-sidebar-refresh-timer nil)
                 (agent-shell-sidebar--file-info (make-hash-table :test 'equal))
                 shown)
            (unless (member file (agent-shell-sidebar--transcripts-for-root root))
              (error "Fixture was not discovered as a regular transcript"))
            (message "DISCOVERED regular=%%S source=%%S scenario=%%S"
                     (file-regular-p file)
                     (symbol-file 'agent-shell-sidebar-open-transcript 'defun) scenario)
            (unless (equal scenario "regular")
              (delete-file file)
              (pcase scenario
                ("rewritten-regular" (write-region "rewritten contents\n" nil file nil 'silent))
                ("directory" (make-directory file))
                ("symlink" (let ((target (concat file ".target")))
                             (write-region "symlink target\n" nil target nil 'silent)
                             (make-symbolic-link target file)))
                ("fifo" (unless (= 0 (call-process "mkfifo" nil nil nil file))
                          (error "mkfifo failed")))))
            (with-temp-buffer
              (agent-shell-sidebar-mode)
              (let ((inhibit-read-only t))
                (insert (propertize "fixture\n" 'agent-shell-sidebar-file file)))
              (goto-char (point-min))
              (cl-letf (((symbol-function 'agent-shell-sidebar--pop-to)
                         (lambda (buffer) (setq shown buffer))))
                (condition-case err
                    (progn
                      (agent-shell-sidebar-open-transcript)
                      (with-current-buffer shown
                        (message "RESULT opened=t view=%%S contents=%%S"
                                 view-mode (buffer-string))))
                  (error (message "RESULT error=%%S text=%%S"
                                  (car err) (error-message-string err))))))
            (when (buffer-live-p shown) (kill-buffer shown))))''' % (
                json.dumps(str(file)), json.dumps(directory + '/'), json.dumps(scenario))
        started = time.monotonic()
        try:
            result = subprocess.run(command + ['--eval', expression], capture_output=True,
                                    text=True, timeout=3)
            record = {'scenario': scenario, 'timeout': False, 'returncode': result.returncode,
                      'output': result.stdout + result.stderr}
        except subprocess.TimeoutExpired as error:
            def decode(value):
                return value.decode() if isinstance(value, bytes) else (value or '')
            record = {'scenario': scenario, 'timeout': True,
                      'output': decode(error.stdout) + decode(error.stderr)}
        record['elapsed_seconds'] = round(time.monotonic() - started, 3)
        results.append(record)
print(json.dumps({'source': str(source), 'source_sha256': hashlib.sha256(source.read_bytes()).hexdigest(),
                  'command_prefix': command, 'results': results}, indent=2))
