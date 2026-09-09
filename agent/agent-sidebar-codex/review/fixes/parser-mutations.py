"""Require the independent oracle to reject each restored defective function."""
import pathlib
import subprocess
import tempfile

here = pathlib.Path(__file__).resolve().parent
review = here.parent
root = review.parents[2]
fixed = (root / 'lisp/agent-sidebar.el').read_text()
baseline = (review / 'baseline-0-agent-sidebar.el').read_text()

def function(text, name):
    start = text.index('(defun ' + name + ' ')
    end = text.index('\n(defun ', start + 1)
    return text[start:end]

for name, selector, label in [
    ('agent-sidebar--codex-user-text', 'ce-plugin-inventory-is-context-not-the-first-prompt', 'context'),
    ('agent-sidebar--codex-parse', 'ce-first-prompt-survives-later-event-from-a-new-turn', 'chronology'),
]:
    mutated = fixed.replace(function(fixed, name), function(baseline, name), 1)
    with tempfile.TemporaryDirectory(prefix='sidebar-parser-mutant-', dir='/private/tmp') as directory:
        source = pathlib.Path(directory) / 'agent-sidebar.el'
        source.write_text(mutated)
        result = subprocess.run([
            'python3', str(review / 'run-review.py'), '--front', str(source),
            '--core', str(root / 'lisp/agent-shell-sidebar.el'),
            '--probe', str(here / 'parser-tests.el'), '--selector', selector,
            '--log', str(here / ('parser-mutant-' + label + '.log')),
        ], capture_output=True, text=True, timeout=120)
    if result.returncode != 1 or ('FAILED  1/1  ' + selector) not in result.stdout:
        raise SystemExit('Mutation was not rejected by the expected oracle: ' + label + '\n' + result.stdout + result.stderr)
    print(label + ': restored baseline function rejected by ' + selector)
