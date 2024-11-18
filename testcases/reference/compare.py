import sys


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


for filename in sys.argv[1:]:
    print("Got result:", parse_file(filename))
