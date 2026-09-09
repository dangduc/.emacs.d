"""Test one sidebar file with no companion library on load-path."""
from pathlib import Path
import argparse
import shutil
import subprocess
import tempfile

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
parser = argparse.ArgumentParser()
parser.add_argument('--source', type=Path, default=ROOT / 'lisp/agent-sidebar.el')
parser.add_argument('--compile', action='store_true')
parser.add_argument('--compiled', action='store_true')
parser.add_argument('--emacs', default='/Applications/Emacs.app/Contents/MacOS/Emacs')
parser.add_argument('--source-dependencies', action='store_true')
parser.add_argument('--log', type=Path, default=HERE / 'source-tests.log')
args = parser.parse_args()

def rename(text):
    return (text.replace('agent-shell-sidebar--ensure-parsed', 'agent-sidebar--ensure-header')
            .replace('agent-shell-sidebar--sidebar-buffer', 'agent-sidebar--sidebar-window')
            .replace('agent-shell-sidebar', 'agent-sidebar'))

with tempfile.TemporaryDirectory(prefix='agent-sidebar-rename-', dir='/private/tmp') as temp:
    work = Path(temp)
    library = work / 'agent-sidebar.el'
    shutil.copy2(args.source, library)
    cmd = [args.emacs, '-Q', '--batch', '-l', 'jka-compr', '-L', str(work)]
    for name in ['shell-maker', 'acp', 'agent-shell']:
        dep = sorted((ROOT / 'elpa/31').glob(name + '-[0-9]*'))[-1]
        if args.source_dependencies:
            target = work / dep.name
            shutil.copytree(dep, target, ignore=shutil.ignore_patterns('*.elc', '*.eln'))
            dep = target
        cmd += ['-L', str(dep)]
    if args.compile or args.compiled:
        compile_result = subprocess.run(cmd + ['--eval', '(setq byte-compile-error-on-warn t)',
                                               '-f', 'batch-byte-compile', str(library)],
                                        capture_output=True, text=True, timeout=120)
        compile_output = compile_result.stdout + compile_result.stderr
        if compile_result.returncode or args.compile:
            args.log.write_text(compile_output)
            print(compile_output)
            if args.compile and not compile_result.returncode:
                shutil.copy2(library.with_suffix('.elc'), ROOT / 'lisp/agent-sidebar.elc')
            raise SystemExit(compile_result.returncode)
    # Preserve original review evidence. Only names change in this temporary
    # replay copy; fixtures, assertions, and mutation controls stay the same.
    for name in ['agent-sidebar-merge', 'agent-sidebar-codex']:
        directory = ROOT / 'agent' / name
        for source in directory.rglob('*'):
            if not source.is_file():
                continue
            if not (source.name.endswith('tests.el') or source.name == 'regressions.el'
                    or source.name.endswith('fixture.py')):
                continue
            dest = work / 'agent' / source.relative_to(ROOT / 'agent')
            dest.parent.mkdir(parents=True, exist_ok=True)
            text = rename(source.read_text()) if source.suffix == '.el' else source.read_text()
            text = text.replace(str(ROOT / 'agent/agent-sidebar-codex'),
                                str(work / 'agent/agent-sidebar-codex'))
            text = text.replace(str(ROOT / 'agent/agent-sidebar-merge'),
                                str(work / 'agent/agent-sidebar-merge'))
            dest.write_text(text)
            shutil.copymode(source, dest)
    fixture = work / 'agent/agent-shell-sidebar/tests/acp-fixture.py'
    fixture.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(ROOT / 'agent/agent-shell-sidebar/tests/acp-fixture.py', fixture)
    suite = work / 'agent/agent-sidebar-codex/review/regressions.el'
    check = subprocess.run(cmd + ['-l', str(library.with_suffix('.elc') if args.compiled else library),
                                   '-l', str(suite), '-l', str(HERE / 'rename-tests.el'),
                                   '-f', 'ert-run-tests-batch-and-exit'],
                           capture_output=True, text=True, timeout=120)
    output = check.stdout + check.stderr
    args.log.write_text(output)
    print(output)
    raise SystemExit(check.returncode)
