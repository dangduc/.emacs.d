"""Record the Codex client's home, then use the existing local ACP fixture."""
import json
import os
from pathlib import Path
import runpy
import sys

with open(sys.argv[1], "a") as stream:
    stream.write(json.dumps({"fixture_environment": {"codex_home": os.environ.get("CODEX_HOME")}}) + "\n")
runpy.run_path(str(Path(__file__).resolve().parent.parent / "agent-shell-sidebar/tests/acp-fixture.py"), run_name="__main__")
