"""Publish the requested merged sources, refusing concurrent Desktop edits."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import tempfile

root = Path(__file__).resolve().parents[2]
expected = {
    "agent-sidebar.el": "f3041ae1f8b7e9cd14e6f5539979de71ab4691e1b22e062d29c55b170ee70b1d",
    "agent-shell-sidebar.el": "4c2e73058c39fe01215d879ffc882005cb980f06f05d4a4e0f72c0461a7546b3",
}
desktop = Path("/Users/ducnguyen/Desktop")
digest = lambda path: hashlib.sha256(path.read_bytes()).hexdigest()
for name, before in expected.items():
    target = desktop / name
    if target.is_symlink() or digest(target) not in {before, digest(root / "lisp" / name)}:
        raise SystemExit(f"Desktop file changed since merge input: {target}")
result = {}
for name in expected:
    target = desktop / name
    with tempfile.NamedTemporaryFile(prefix=".sidebar-merge-", dir=desktop, delete=False) as stream:
        temporary = Path(stream.name)
    try:
        shutil.copy2(root / "lisp" / name, temporary)
        os.replace(temporary, target)
    finally:
        temporary.unlink(missing_ok=True)
    assert digest(target) == digest(root / "lisp" / name)
    result[str(target)] = digest(target)
(root / "agent/agent-sidebar-merge/desktop-sync.json").write_text(json.dumps(result, indent=2) + "\n")
print(json.dumps(result, indent=2))
