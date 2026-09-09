"""Challenge the combined suite using private runtime mutations."""
from pathlib import Path
import argparse
import hashlib
import json
import subprocess

directory = Path(__file__).resolve().parent
review = directory.parent
parser = argparse.ArgumentParser()
parser.add_argument('--controls-only', action='store_true')
args = parser.parse_args()
source_path = directory / 'reviewed.el'
source = source_path.read_text()
assert hashlib.sha256(source_path.read_bytes()).hexdigest() == '629a0b61dc25a1b9766936b34b655aea9e1775c57f0f2d98ebb9ef00fea1b6ed'
replacements = {
    'unbounded': ('(insert-file-contents file nil 0 8192)', '(insert-file-contents file)'),
    'separator-only': (r'"^\\(?:---[ \t]*$\\|## \\)"', r'"^---[ \t]*$"'),
    'partial-lines': ('(goto-char (point-max))\n                                     (line-beginning-position))', '(goto-char (point-max))\n                                     (point-max))'),
    'trim-directory': ('(unless (eq (cdr field) :cwd)', '(when t'),
}
sources = {'current': source_path}
for name, (before, after) in replacements.items():
    assert source.count(before) == 1, (name, source.count(before))
    path = directory / f'contrarian-evidence-mutant-{name}.el'
    path.write_text(source.replace(before, after))
    sources[name] = path
results = ([r for r in json.loads((directory / 'contrarian-evidence-mutation-results.json').read_text())
            if r['group'] == 'combined'] if args.controls_only else [])
for name, path in sources.items():
    for group in ['controls', 'combined']:
        if args.controls_only and group == 'combined':
            continue
        log = directory / f'contrarian-evidence-{group}-{name}.log'
        if group == 'combined':
            command = ['python3', str(directory / 'contrarian-evidence-runner.py'), '--source', str(path), '--log', str(log)]
        else:
            command = ['python3', str(review / 'run-probe.py'), str(directory / 'contrarian-evidence-probe.el'), '--source', str(path), '--ert', '--log', str(log)]
        result = subprocess.run(command, capture_output=True, text=True, timeout=110)
        output = result.stdout + result.stderr
        record = dict(variant=name, group=group, source_sha256=hashlib.sha256(path.read_bytes()).hexdigest(),
                      command=command, exit=result.returncode,
                      summary=[line for line in output.splitlines() if line.startswith(('Ran ', '   FAILED', 'CE2 SOURCE'))])
        results.append(record)
        print(json.dumps(record), flush=True)
        (directory / 'contrarian-evidence-mutation-results.json').write_text(json.dumps(results, indent=2))
        assert result.returncode == (0 if name == 'current' else 1)
inputs = [source_path, directory / 'contrarian-evidence-runner.py', directory / 'contrarian-evidence-probe.el', Path(__file__).resolve()]
(directory / 'contrarian-evidence-provenance.json').write_text(json.dumps({str(p): hashlib.sha256(p.read_bytes()).hexdigest() for p in inputs}, indent=2))
