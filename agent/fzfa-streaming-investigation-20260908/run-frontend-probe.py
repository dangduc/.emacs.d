from pathlib import Path
import json
import os
import pty
import select
import signal
import struct
import sys
import termios
import fcntl
import time

out = Path(__file__).resolve().parent
root = out.parent.parent
frontend = sys.argv[1]
mode = sys.argv[2] if len(sys.argv) > 2 else "flood"
variant = sys.argv[3] if len(sys.argv) > 3 else "current"
layout = sys.argv[4] if len(sys.argv) > 4 else "single"
stem = f"frontend-{frontend}-{mode}-{variant}" + ("-multi" if layout == "multi" else "")
env = os.environ.copy()
env.update(TERM="xterm-256color", PROBE_MODE=mode, PROBE_QUERY="alpha",
           PROBE_FRONTEND=frontend, PROBE_LAYOUT=layout,
           PROBE_OUTPUT=str(out / (stem + ".json")))
libraries = [root / "vendor/fzf-native", root / "vendor/fzfa"]
if variant != "current":
    libraries.append(out / variant)
for package in ("ivy", "helm", "helm-core", "async", "vertico"):
    libraries.extend(sorted((root / "elpa/31").glob(package + "-[0-9]*")))
cmd = ["/Applications/Emacs.app/Contents/MacOS/Emacs", "-Q", "-nw"]
for library in libraries:
    cmd.extend(["-L", str(library)])
cmd.extend(["-l", str(out / "frontend-probe.el")])
(out / (stem + ".json")).unlink(missing_ok=True)
pid, fd = pty.fork()
if pid == 0:
    os.execve(cmd[0], cmd, env)
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack('HHHH', 40, 140, 0, 0))
started = time.monotonic()
transcript = bytearray()
status = None
sent = []
schedule = [(0.9, "bravo"), (1.8, "zzzznotfound"), (2.7, "alpha"), (3.6, "")] if mode == "queries" else []
try:
    while time.monotonic() - started < 12:
        elapsed = time.monotonic() - started
        if schedule and elapsed >= schedule[0][0]:
            _, query = schedule.pop(0)
            os.write(fd, b"\x01\x0b" + query.encode())
            sent.append({"seconds": elapsed, "query": query})
        ready, _, _ = select.select([fd], [], [], 0.025)
        if ready:
            try:
                chunk = os.read(fd, 65536)
            except OSError:
                chunk = b""
            if chunk:
                transcript.extend(chunk)
                (out / (stem + ".terminal.log")).write_bytes(transcript)
        ended, child_status = os.waitpid(pid, os.WNOHANG)
        if ended:
            status = child_status
            break
    if status is None:
        os.kill(pid, signal.SIGKILL)
        for _ in range(20):
            ended, child_status = os.waitpid(pid, os.WNOHANG)
            if ended:
                status = child_status
                break
            time.sleep(0.05)
finally:
    os.close(fd)
    (out / (stem + ".terminal.log")).write_bytes(transcript)
result_file = out / (stem + ".json")
if not result_file.exists():
    print(transcript[-4000:].decode(errors="replace"))
    raise SystemExit("No probe result; child status: " + str(status))
data = json.loads(result_file.read_text())
samples = data.pop("samples")
data.update(child_status=status, variant=variant, sent=sent)
data["first_display"] = next((s["seconds"] for s in samples
                              if s["candidates"] and s["display_has_records"]), None)
data["reader_done_at"] = next((s["seconds"] for s in samples if s["reader_done"]), None)
data["visible_samples_before_eof"] = sum(s["candidates"] > 0 and s["display_has_records"]
                                          and not s["reader_done"] for s in samples)
if mode == "queries":
    data["queries"] = {}
    for query in ("alpha", "bravo", "zzzznotfound", ""):
        matching = [s for s in samples if s["input"] == query]
        last = matching[-1] if matching else {}
        data["queries"][query] = {k: last.get(k) for k in
                                  ("seconds", "candidates", "alpha", "bravo", "display_has_records")}
(out / (stem + "-summary.json")).write_text(json.dumps(data, indent=2) + "\n")
print(json.dumps(data))
if data["error"] or status != 0:
    raise SystemExit(1)
