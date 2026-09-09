"""Record only non-secret review fields from a local ACP child."""
import json
import os
from pathlib import Path
import sys
import time
Path(sys.argv[1]).write_text(json.dumps({"home": os.environ.get("CODEX_HOME"), "tag": os.environ.get("SIDEBAR_REVIEW_TAG"), "cwd": os.getcwd()}))
time.sleep(30)
