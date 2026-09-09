#!/usr/bin/env python3
"""Record the actual ghostel child launch and stay alive for the reuse check."""
import json
import os
from pathlib import Path
import sys
import time

Path(os.environ["SIDEBAR_CODEX_PROBE_OUTPUT"]).write_text(json.dumps({
    "argv": sys.argv[1:], "cwd": os.getcwd(), "codex_home": os.environ.get("CODEX_HOME")
}))
time.sleep(12)
