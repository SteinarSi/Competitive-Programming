from sys import stdin
from math import pi, log10, e

for line in stdin:
    n = int(line.rstrip())
    print(1 if n <= 1 else int(n * log10(n / e) + 0.5 * log10(2 * pi * n)) + 1)
