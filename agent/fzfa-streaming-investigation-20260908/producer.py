import sys
import time

mode = sys.argv[1]
if mode == "paused":
    sys.stdout.write("doc-0000000.txt:1:alpha\ndoc-0000001.txt:1:alpha\n")
    sys.stdout.flush()
    time.sleep(2)
    sys.stdout.write("doc-0000002.txt:1:alpha\n")
    sys.stdout.flush()
elif mode in ("continuous", "flood"):
    for batch in range(400 if mode == "continuous" else 2000):
        start = batch * 1000
        sys.stdout.write("".join(f"doc-{i:07d}.txt:1:alpha beta gamma\n"
                                 for i in range(start, start + 1000)))
        sys.stdout.flush()
        if mode == "continuous":
            time.sleep(0.005)
