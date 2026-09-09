"""Record a local terminal launch without starting an external agent."""
import json
import os
import sys
from pathlib import Path

Path(sys.argv[1]).write_text(json.dumps({"argv": sys.argv[2:], "cwd": os.getcwd()}))
