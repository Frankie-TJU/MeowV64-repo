import sys
import numpy as np


def parse_file(filename):
    res = []
    content = open(filename, "rb").read().decode("utf-8", errors="ignore")
    for line in content.splitlines():
        if line.startswith("Result: ["):
            temp = []
            parts = line.strip().split("[")[1].removesuffix("]").split(",")
            for part in parts:
                temp.append(float(part.strip()))
            res = temp
    return res


prev = None
for filename in sys.argv[1:]:
    res = np.array(parse_file(filename))
    print("Got result:", res)
    if prev is not None:
        print("Mean squared error:", np.mean((res - prev) ** 2))
    prev = res
