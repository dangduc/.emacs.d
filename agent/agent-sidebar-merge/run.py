"""Compile or test the merged sidebar with isolated Emacs dependency paths."""
from pathlib import Path
import argparse,subprocess,tempfile,shutil
root=Path(__file__).resolve().parents[2]
parser=argparse.ArgumentParser()
parser.add_argument('--compile',action='store_true')
parser.add_argument('--compiled',action='store_true')
parser.add_argument('--emacs',default='/Applications/Emacs.app/Contents/MacOS/Emacs')
parser.add_argument('--source-dependencies',action='store_true')
parser.add_argument('--probe',type=Path,default=Path(__file__).parent/'tests.el')
parser.add_argument('--log',type=Path,default=Path(__file__).parent/'tests.log')
args=parser.parse_args()
cmd=[args.emacs,'-Q','--batch','-l','jka-compr','-L',str(root/'lisp')]
with tempfile.TemporaryDirectory(prefix='agent-sidebar-deps-',dir='/private/tmp') as temp:
    for name in ['shell-maker','acp','agent-shell']:
        directory=sorted((root/'elpa/31').glob(name+'-[0-9]*'))[-1]
        if args.source_dependencies:
            target=Path(temp)/directory.name
            shutil.copytree(directory,target,ignore=shutil.ignore_patterns('*.elc','*.eln'))
            directory=target
        cmd+=['-L',str(directory)]
    if args.compile:
        cmd+=['--eval','(setq byte-compile-error-on-warn t)','-f','batch-byte-compile',str(root/'lisp/agent-shell-sidebar.el'),str(root/'lisp/agent-sidebar.el')]
    else:
        ext='.elc' if args.compiled else '.el'
        for name in ['agent-shell-sidebar','agent-sidebar']:
            cmd+=['-l',str(root/'lisp'/str(name+ext))]
        cmd+=['-l',str(args.probe.resolve()),'-f','ert-run-tests-batch-and-exit']
    r=subprocess.run(cmd,capture_output=True,text=True,timeout=120)
args.log.write_text(r.stdout+r.stderr)
print(r.stdout+r.stderr)
raise SystemExit(r.returncode)
