"""Assert independently collected changed-file controls and the parser control."""
from pathlib import Path
import json

folder = Path(__file__).resolve().parent
frozen = json.loads((folder / 'contrarian-workflow-view-frozen.json').read_text())
fixed = json.loads((folder / 'contrarian-workflow-view-fixed.json').read_text())
for collection in [frozen, fixed]:
    cases = {case['scenario']: case for case in collection['results']}
    for scenario in ['regular', 'rewritten-regular']:
        case = cases[scenario]
        assert not case['timeout'] and case['returncode'] == 0
        assert 'RESULT opened=t view=t' in case['output']
    assert 'rewritten contents' in cases['rewritten-regular']['output']
    assert 'Cannot read transcript:' in cases['missing']['output']
    assert not cases['missing']['timeout']
old = {case['scenario']: case for case in frozen['results']}
new = {case['scenario']: case for case in fixed['results']}
assert old['fifo']['timeout']
for scenario in ['fifo', 'directory', 'symlink']:
    assert not new[scenario]['timeout'] and new[scenario]['returncode'] == 0
    assert 'RESULT error=user-error' in new[scenario]['output']
    assert 'Not a regular transcript file:' in new[scenario]['output']
parser = json.loads((folder / 'contrarian-workflow-parser-fifo.json').read_text())
assert not parser['timed_out'] and parser['returncode'] == 0
assert 'discovered=nil' in parser['output']
assert 'Not a regular transcript file' in parser['output']
print('PASS: ordinary/replaced regular files still open; changed special files reject; old FIFO blocks.')
print('PASS: parser excludes an initial FIFO and returns an error without blocking.')
print('Source fingerprints:', frozen['source_sha256'], fixed['source_sha256'])
