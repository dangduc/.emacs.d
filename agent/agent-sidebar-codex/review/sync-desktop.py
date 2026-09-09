"""Copy reviewed sidebar sources to Desktop, preserving concurrent edits."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import tempfile

here = Path(__file__).resolve().parent
root = here.parents[2]
inputs = json.loads((here / "inputs.json").read_text())
pairs = [(root / "lisp" / name, Path("/Users/ducnguyen/Desktop") / name)
         for name in ["agent-sidebar.el", "agent-shell-sidebar.el"]]

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

for source, target in pairs:
    if target.is_symlink() or digest(target) not in {inputs[str(target)], digest(source)}:
        raise SystemExit(f"Desktop file changed after review snapshot; preserve it: {target}")
for source, target in pairs:
    if digest(source) == digest(target):
        continue
    with tempfile.NamedTemporaryFile(prefix=".sidebar-review-", dir=target.parent, delete=False) as stream:
        temporary = Path(stream.name)
    try:
        shutil.copy2(source, temporary)
        os.replace(temporary, target)
    finally:
        temporary.unlink(missing_ok=True)
    assert digest(source) == digest(target)
result = {str(target): digest(target) for _, target in pairs}
(here / "desktop-sync.json").write_text(json.dumps(result, indent=2) + "\n")
print(json.dumps(result, indent=2))
