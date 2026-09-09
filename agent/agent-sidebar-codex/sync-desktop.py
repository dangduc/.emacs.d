"""Update the Desktop sidebar while preserving concurrent edits."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import tempfile

here = Path(__file__).resolve().parent
source = here.parents[1] / "lisp/agent-sidebar.el"
target = Path("/Users/ducnguyen/Desktop/agent-sidebar.el")
expected = json.loads((here / "inputs.json").read_text())[str(target)]
digest = lambda path: hashlib.sha256(path.read_bytes()).hexdigest()
if target.is_symlink() or digest(target) not in {expected, digest(source)}:
    raise SystemExit("Desktop sidebar changed after the input snapshot; preserve that edit")
with tempfile.NamedTemporaryFile(prefix=".sidebar-codex-", dir=target.parent, delete=False) as stream:
    temporary = Path(stream.name)
try:
    shutil.copy2(source, temporary)
    os.replace(temporary, target)
finally:
    temporary.unlink(missing_ok=True)
assert digest(source) == digest(target)
result = {str(target): digest(target)}
(here / "desktop-sync.json").write_text(json.dumps(result, indent=2) + "\n")
print(json.dumps(result, indent=2))
