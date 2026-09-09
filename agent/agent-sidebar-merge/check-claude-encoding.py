"""Check local Claude directory encoding; report counts, never transcript text."""
import json
import re
from pathlib import Path

checked = missing = mismatch = 0
for file in (Path.home() / ".claude/projects").glob("*/*.jsonl"):
    if file.is_symlink() or not file.is_file():
        continue
    with file.open("rb") as stream:
        data = stream.read(65536)
    cwd = None
    for line in data.splitlines():
        try:
            record = json.loads(line)
        except (ValueError, UnicodeError):
            continue
        if isinstance(record, dict) and isinstance(record.get("cwd"), str):
            cwd = record["cwd"]
            break
    if cwd is None:
        missing += 1
    else:
        checked += 1
        mismatch += re.sub(r"[^\w-]", "-", cwd.rstrip("/")).replace("_", "-") != file.parent.name
    if checked + missing >= 50:
        break
print(json.dumps({"checked": checked, "no_cwd_in_prefix": missing, "encoding_mismatches": mismatch}))
