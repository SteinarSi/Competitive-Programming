import sys
from collections import Counter

xs = sys.stdin.readlines()

kinds = sorted(set(xs))
count = Counter(xs)
scale = 100 / len(xs)

for kind in kinds:
    print(f"{kind[:-1]} {count[kind] * scale}")
