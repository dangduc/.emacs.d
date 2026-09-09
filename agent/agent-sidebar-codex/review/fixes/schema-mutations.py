"""Restore each permissive block guard and require its regression to fail."""
import pathlib
import subprocess
import tempfile

here = pathlib.Path(__file__).resolve().parent
review = here.parent
root = review.parents[2]
fixed = (root / 'lisp/agent-sidebar.el').read_text()
for function, selector, label in [
    ('agent-sidebar--codex-user-text', 'ce2-unexpected-codex-content-shape-does-not-poison-valid-session', 'codex'),
    ('agent-sidebar--content-text', 'ce2-unexpected-claude-content-shape-does-not-poison-valid-session', 'claude'),
]:
    start = fixed.index('(defun ' + function + ' ')
    end = fixed.index('\n(defun ', start + 1)
    body = fixed[start:end]
    assert body.count('(proper-list-p block)') == 1
    mutated = fixed[:start] + body.replace('(proper-list-p block)', '(listp block)') + fixed[end:]
    with tempfile.TemporaryDirectory(prefix='sidebar-schema-mutant-', dir='/private/tmp') as directory:
        source = pathlib.Path(directory) / 'agent-sidebar.el'
        source.write_text(mutated)
        result = subprocess.run([
            'python3', str(review / 'run-review.py'), '--front', str(source),
            '--core', str(root / 'lisp/agent-shell-sidebar.el'),
            '--probe', str(here / 'schema-tests.el'), '--selector', selector,
            '--log', str(here / ('schema-mutant-' + label + '.log')),
        ], capture_output=True, text=True, timeout=120)
    if result.returncode != 1 or ('FAILED  1/1  ' + selector) not in result.stdout or 'wrong-type-argument listp' not in result.stdout:
        raise SystemExit('Mutation did not fail at the expected schema boundary: ' + label + '\n' + result.stdout + result.stderr)
    print(label + ': restored listp guard rejected by ' + selector)
