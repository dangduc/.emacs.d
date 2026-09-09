"""Offline ACP fixture with a stable UUID for a newly created Codex session."""
import json
import sys

for line in sys.stdin:
    request = json.loads(line)
    with open(sys.argv[1], "a") as log:
        log.write(json.dumps(request) + "\n")
    if "id" not in request:
        continue
    method = request.get("method")
    if method == "initialize":
        result = {"protocolVersion": 1, "agentCapabilities": {"loadSession": True},
                  "authMethods": [], "agentInfo": {"name": "ownership-fixture", "version": "1"}}
    elif method == "session/load":
        result = {}
    elif method == "session/new":
        result = {"sessionId": "23456789-1234-7123-8123-123456789abc"}
    else:
        raise RuntimeError("Unexpected method: " + str(method))
    print(json.dumps({"jsonrpc": "2.0", "id": request["id"], "result": result}), flush=True)
