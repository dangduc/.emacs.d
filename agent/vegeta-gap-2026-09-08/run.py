"""Capture source inventory and run the same contract checks on both libraries."""
from pathlib import Path
import difflib
import hashlib
import json
import os
import subprocess

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
EMACS = '/Applications/Emacs.app/Contents/MacOS/Emacs'
local = HERE / 'local-agent-sidebar.el'
vegeta = HERE / 'vegeta/vegeta.el'
cmd = [EMACS, '-Q', '--batch', '-l', 'jka-compr']
for package in ['shell-maker', 'acp', 'agent-shell']:
    cmd += ['-L', str(sorted((ROOT / 'elpa/31').glob(package + '-[0-9]*'))[-1])]

result = subprocess.run(cmd + ['-l', str(HERE / 'inventory.el'), str(local), str(vegeta)],
                        capture_output=True, text=True, check=True, timeout=30)
inventory = json.loads(result.stdout)
(HERE / 'inventory.json').write_text(json.dumps(inventory, indent=2) + '\n')
tables = [{entry['name']: entry for entry in inventory[str(path)]} for path in [local, vegeta]]
a, b = tables
summary = {
    'local_sha256': hashlib.sha256(local.read_bytes()).hexdigest(),
    'vegeta_sha256': hashlib.sha256(vegeta.read_bytes()).hexdigest(),
    'local_lines': len(local.read_text().splitlines()),
    'vegeta_lines': len(vegeta.read_text().splitlines()),
    'local_definitions': len(a), 'vegeta_definitions': len(b),
    'local_only': sorted(a.keys() - b.keys()),
    'vegeta_only': sorted(b.keys() - a.keys()),
    'identical_forms': sorted(name for name in a.keys() & b.keys() if a[name]['form'] == b[name]['form']),
    'changed_forms': sorted(name for name in a.keys() & b.keys() if a[name]['form'] != b[name]['form']),
}
(HERE / 'comparison.json').write_text(json.dumps(summary, indent=2) + '\n')
(HERE / 'normalized.diff').write_text(''.join(difflib.unified_diff(
    vegeta.read_text().splitlines(True), local.read_text().replace('agent-sidebar', 'vegeta').splitlines(True),
    fromfile='GitHub/vegeta.el', tofile='local/agent-sidebar.el (namespace normalized)')))
for label, path, prefix in [('local', local, 'agent-sidebar'), ('vegeta', vegeta, 'vegeta')]:
    result = subprocess.run(cmd + ['-l', str(path), '-l', str(HERE / 'gap-tests.el'),
                                   '-f', 'ert-run-tests-batch-and-exit'],
                            env=dict(os.environ, GAP_PREFIX=prefix),
                            capture_output=True, text=True, timeout=60)
    output = result.stdout + result.stderr
    (HERE / (label + '-tests.log')).write_text(output)
    print(label, 'exit', result.returncode)
    print('\n'.join(line for line in output.splitlines()
                    if line.startswith(('Ran ', '   passed', '   FAILED', 'GAP '))))
print(json.dumps({key: value for key, value in summary.items()
                  if key not in ['local_only', 'changed_forms']}, indent=2))
