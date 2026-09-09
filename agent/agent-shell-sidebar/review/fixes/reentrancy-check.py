"""Compile a temporary candidate and run its focused regressions."""
from pathlib import Path
import shutil
import subprocess
import tempfile

here = Path(__file__).resolve().parent
root = here.parents[3]
with tempfile.TemporaryDirectory(prefix='sidebar-reentrancy-compile-', dir='/private/tmp') as temp:
    source = Path(temp)/'agent-shell-sidebar.el'
    shutil.copyfile(here/'reentrancy.el', source)
    command = ['/Applications/Emacs.app/Contents/MacOS/Emacs','-Q','--batch','-l','jka-compr']
    for name in ['shell-maker','acp','agent-shell']:
        command += ['-L',str(sorted((root/'elpa/31').glob(name+'-[0-9]*'))[-1])]
    result = subprocess.run(command+['--eval','(setq byte-compile-error-on-warn t)','-f','batch-byte-compile',str(source)], capture_output=True,text=True,timeout=60)
    (here/'reentrancy-compile.log').write_text(result.stdout+result.stderr)
    if result.returncode:
        raise SystemExit(result.returncode)
    result = subprocess.run(['python3',str(here.parent/'run-probe.py'),str(here/'reentrancy-tests.el'),'--source',str(source.with_suffix('.elc')),'--ert','--log',str(here/'reentrancy-compiled-tests.log')],capture_output=True,text=True,timeout=90)
    print(result.stdout+result.stderr)
    raise SystemExit(result.returncode)
