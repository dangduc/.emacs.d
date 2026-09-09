"""Probe real transcript viewing after discovery, with bounded subprocesses."""
import argparse
import json
from pathlib import Path
import subprocess
import tempfile
import time

parser = argparse.ArgumentParser()
parser.add_argument("--source", type=Path, required=True)
parser.add_argument("--log", type=Path, required=True)
args = parser.parse_args()
root = Path(__file__).resolve().parents[4]
base = ["/Applications/Emacs.app/Contents/MacOS/Emacs", "-Q", "--batch", "-l", "jka-compr"]
for dependency in ["shell-maker", "acp", "agent-shell"]:
    base += ["-L", str(sorted((root / "elpa/31").glob(dependency + "-[0-9]*"))[-1])]
base += ["-l", str(args.source.resolve())]
results = []

for replacement in ["regular", "fifo"]:
    with tempfile.TemporaryDirectory(prefix="sidebar-torvalds2-open-") as directory:
        transcript = Path(directory) / ".agent-shell/transcripts/read.md"
        transcript.parent.mkdir(parents=True)
        transcript.write_text("**Agent:** Fixture\n---\n## User\nLocal viewer control\n")
        expression = r'''(let* ((root ROOT)
                               (file FILE)
                               (replacement REPLACEMENT)
                               (default-directory root)
                               (agent-shell-sidebar-extra-project-roots (list root))
                               (agent-shell-sidebar-refresh-timer nil)
                               (projectile-known-projects nil)
                               (enable-local-variables nil)
                               (enable-local-eval nil))
          (cl-letf (((symbol-function 'project-known-project-roots) (lambda () nil))
                    ((symbol-function 'agent-shell-buffers) (lambda () nil))
                    ((symbol-function 'agent-shell-sidebar--pop-to) #'ignore))
            (with-temp-buffer
              (agent-shell-sidebar-mode)
              (agent-shell-sidebar-refresh)
              (goto-char (point-min))
              (let ((match (text-property-search-forward 'agent-shell-sidebar-file file #'equal)))
                (unless match (error "Fixture was not discovered"))
                (goto-char (prop-match-beginning match)))
              (when (equal replacement "fifo")
                (delete-file file)
                (unless (zerop (call-process "mkfifo" nil nil nil file))
                  (error "Could not create FIFO")))
              (message "BEFORE-OPEN discovered=t replacement=%s modes=%s readable=%S"
                       replacement (file-attribute-modes (file-attributes file)) (file-readable-p file))
              (condition-case err
                  (progn
                    (agent-shell-sidebar-open-transcript)
                    (message "OPEN-RETURNED view=%S"
                             (buffer-local-value 'view-mode (get-file-buffer file))))
                (error (message "OPEN-REFUSED %S" err))))))'''
        expression = expression.replace("ROOT", json.dumps(directory + "/"))
        expression = expression.replace("FILE", json.dumps(str(transcript)))
        expression = expression.replace("REPLACEMENT", json.dumps(replacement))
        started = time.monotonic()
        try:
            result = subprocess.run(base + ["--eval", expression], text=True, capture_output=True, timeout=4)
            payload = {"case": replacement, "timed_out": False,
                       "returncode": result.returncode, "output": result.stdout + result.stderr}
        except subprocess.TimeoutExpired as error:
            def decode(value):
                return value.decode() if isinstance(value, bytes) else value or ""
            payload = {"case": replacement, "timed_out": True,
                       "output": decode(error.stdout) + decode(error.stderr)}
        payload["elapsed_seconds"] = round(time.monotonic() - started, 3)
        results.append(payload)

output = "\n".join(json.dumps(result) for result in results) + "\n"
args.log.write_text(output)
print(output, end="")
control, fifo = results
assert not control["timed_out"] and "OPEN-RETURNED view=t" in control["output"], "Regular viewing control failed"
assert not fifo["timed_out"] and "OPEN-REFUSED" in fifo["output"], "Viewing must reject a discovered path replaced by a FIFO"
