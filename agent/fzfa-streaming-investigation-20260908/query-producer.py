import sys
import time

for batch in range(120):
    start = batch * 1000
    sys.stdout.write("".join(
        f"doc-{i:07d}.txt:1:{'alpha' if i % 2 else 'bravo'}\n"
        for i in range(start, start + 1000)))
    sys.stdout.flush()
    time.sleep(0.025)
