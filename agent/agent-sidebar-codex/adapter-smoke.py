"""Check the installed ACP adapter's initialize response in an isolated home."""
import json
import os
from pathlib import Path
import select
import shutil
import signal
import subprocess
import tempfile
import time

here = Path(__file__).resolve().parent
command = shutil.which("codex-acp")
assert command
with tempfile.TemporaryDirectory(prefix="codex-acp-sidebar-", dir="/private/tmp") as temporary:
    env = dict(os.environ)
    env["CODEX_HOME"] = temporary
    for key in ["OPENAI_API_KEY", "CODEX_API_KEY", "DEFAULT_AUTH_REQUEST", "CODEX_CONFIG", "MODEL_PROVIDER"]:
        env.pop(key, None)
    with tempfile.TemporaryFile() as errors:
        process = subprocess.Popen([command], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                   stderr=errors, env=env, cwd=temporary, start_new_session=True)
        try:
            request = {"jsonrpc": "2.0", "id": 1, "method": "initialize",
                       "params": {"protocolVersion": 1, "clientCapabilities": {},
                                  "clientInfo": {"name": "sidebar-check", "version": "1"}}}
            process.stdin.write((json.dumps(request) + "\n").encode())
            process.stdin.flush()
            deadline = time.monotonic() + 15
            response = None
            while time.monotonic() < deadline and process.poll() is None:
                ready, _, _ = select.select([process.stdout], [], [], 0.5)
                if ready:
                    line = process.stdout.readline()
                    if not line:
                        break
                    item = json.loads(line)
                    if item.get("id") == 1:
                        response = item
                        break
            assert response and "result" in response, "Adapter did not initialize"
            result = response["result"]
            assert result["agentCapabilities"]["loadSession"]
            output = {"executable": command, "agentInfo": result.get("agentInfo"),
                      "protocolVersion": result["protocolVersion"], "loadSession": True,
                      "isolated_home": True, "prompts_sent": 0}
            (here / "adapter-smoke.json").write_text(json.dumps(output, indent=2) + "\n")
            print(json.dumps(output, indent=2))
        finally:
            if process.poll() is None:
                os.killpg(process.pid, signal.SIGTERM)
                try:
                    process.wait(timeout=3)
                except subprocess.TimeoutExpired:
                    os.killpg(process.pid, signal.SIGKILL)
                    process.wait()
