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
mode = sys.argv[1] if len(sys.argv) > 1 else "paused"
query = sys.argv[2] if len(sys.argv) > 2 else ""
variant = sys.argv[3] if len(sys.argv) > 3 else "current"
fzfa_dir = root / "vendor/fzfa" if variant == "current" else out / variant
stem = "vertico-" + mode + "-" + (query or "empty") + "-" + variant
env = os.environ.copy()
env.update(TERM="xterm-256color", PROBE_MODE=mode, PROBE_QUERY=query,
           PROBE_OUTPUT=str(out / (stem + ".json")))
cmd = ["/Applications/Emacs.app/Contents/MacOS/Emacs", "-Q", "-nw",
       "-L", str(root / "vendor/fzf-native"),
       "-L", str(fzfa_dir),
       "-L", str(root / "elpa/31/vertico-20260821.1200"),
       "-l", str(out / "vertico-probe.el")]
(out / (stem + ".json")).unlink(missing_ok=True)
pid, fd = pty.fork()
if pid == 0:
    os.execve(cmd[0], cmd, env)
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack('HHHH', 40, 140, 0, 0))
started = time.monotonic()
transcript = bytearray()
status = None
try:
    while time.monotonic() - started < 12:
        ready, _, _ = select.select([fd], [], [], 0.1)
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
data["child_status"] = status
data["variant"] = variant
data["first_vertico"] = next((s["seconds"] for s in samples
                              if s["vertico_candidates"]), None)
data["reader_done_at"] = next((s["seconds"] for s in samples
                               if s["reader_done"]), None)
data["visible_samples_before_eof"] = sum(s["vertico_candidates"] > 0
                                          and not s["reader_done"] for s in samples)
print(json.dumps(data))
