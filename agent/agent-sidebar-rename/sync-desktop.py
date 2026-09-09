"""Replace the Desktop delivery after preserving and comparing its old copy."""
from pathlib import Path
import hashlib
import json
import shutil

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
source = ROOT / 'lisp/agent-sidebar.el'
desktop = Path('/Users/ducnguyen/Desktop/agent-sidebar.el')
before = HERE / 'before-agent-sidebar.el'
if desktop.read_bytes() not in (before.read_bytes(), source.read_bytes()):
    raise SystemExit('Desktop source changed independently; inspect before replacement')
if desktop.read_bytes() == before.read_bytes():
    shutil.copy2(desktop, HERE / 'before-desktop-agent-sidebar.el')
shutil.copy2(source, desktop)
assert desktop.read_bytes() == source.read_bytes()
result = {str(path): hashlib.sha256(path.read_bytes()).hexdigest()
          for path in (source, desktop)}
(HERE / 'desktop-sync.json').write_text(json.dumps(result, indent=2) + '\n')
print(json.dumps(result, indent=2))
