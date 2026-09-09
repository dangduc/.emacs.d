"""Exercise actual prefix input in isolated terminal Emacs, not the live editor."""
from pathlib import Path
import argparse
import json
import os
import pty
import select
import signal
import tempfile
import time

here = Path(__file__).resolve().parent
root = here.parents[3]
parser = argparse.ArgumentParser()
parser.add_argument('--source', type=Path, default=here/'reviewed.el')
args = parser.parse_args()
outputs = []

def run_case(key, label):
    with tempfile.TemporaryDirectory(prefix='sidebar-kingsbury-prefix-', dir='/private/tmp') as tmp:
        project = Path(tmp)/'project'
        transcript = project/'.agent-shell/transcripts/one.md'
        transcript.parent.mkdir(parents=True)
        transcript.write_text('**Agent:** Mock\n---\n## User\n\nPrefix schedule\n')
        log = Path(tmp)/'events.log'
        command = ['/Applications/Emacs.app/Contents/MacOS/Emacs', '-Q', '-nw', '-l', 'jka-compr']
        for name in ['shell-maker','acp','agent-shell']:
            command += ['-L',str(sorted((root/'elpa/31').glob(name+'-[0-9]*'))[-1])]
        command += ['-l',str(args.source.resolve()),'-l',str(here/'kingsbury-prefix.el')]
        pid, fd = pty.fork()
        if pid == 0:
            env = dict(os.environ, TERM='xterm-256color', LANG='en_US.UTF-8', LC_ALL='en_US.UTF-8', KINGSBURY_ROOT=str(project), KINGSBURY_LOG=str(log))
            os.execve(command[0], command, env)
        terminal = bytearray()
        def events():
            return log.read_text() if log.exists() else ''
        def drain_until(deadline, predicate=lambda:False):
            while time.monotonic() < deadline:
                if predicate():
                    return True
                ready,_,_ = select.select([fd],[],[],0.02)
                if ready:
                    try:
                        terminal.extend(os.read(fd,65536))
                    except OSError:
                        break
            return predicate()
        try:
            if not drain_until(time.monotonic()+4,lambda:'ARMED' in events()):
                raise RuntimeError('Emacs did not arm the pending timer: '+events()+' terminal='+repr(bytes(terminal[-6000:])))
            before = events()
            started = time.monotonic()
            os.write(fd,key)
            drain_until(started+1.2)
            after = events()
            os.write(fd,b'\x07')
            drain_until(time.monotonic()+0.6)
            final = events()
            return dict(case=label, input_wait_seconds=round(time.monotonic()-started-0.6,3), tick_before_input='TICK' in before, tick_after_input='TICK' in after, tick_after_abort='TICK' in final, events_before=before, events_after=after, events_final=final)
        finally:
            os.kill(pid,signal.SIGKILL)
            # Never block the review if terminal Emacs stalls during exit.
            deadline = time.monotonic()+0.5
            while time.monotonic() < deadline:
                if os.waitpid(pid,os.WNOHANG)[0]:
                    break
                time.sleep(0.02)
            os.close(fd)

for key,label in [(b'n','completed-command-control'),(b'\x18','unfinished-C-x-prefix')]:
    try:
        result=run_case(key,label)
    except RuntimeError as error:
        result=dict(case=label, startup_error=str(error), conclusion="Input schedule not reached")
        outputs.append(result)
        print(json.dumps(result),flush=True)
        break
    outputs.append(result)
    print(json.dumps(result),flush=True)
(here/'kingsbury-prefix.log').write_text('\n'.join(json.dumps(x) for x in outputs)+'\n')
if len(outputs) != 2 or any('startup_error' in x for x in outputs):
    raise SystemExit(2)
assert outputs[0]['tick_after_input'], 'Completed-command control did not resume parsing'
assert outputs[1]['tick_after_input'], 'Prefix input reset native idle age without resetting the pending timer threshold'
