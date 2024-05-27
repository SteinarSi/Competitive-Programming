from queue import PriorityQueue
from collections import Counter

COSTS = [1,3,3,5,5,5,7,7,7,7,7,9,9,9,9,9,9,9,9,11,11,11,11,11,11,11]
n = 0
total = 0
count = sorted(Counter((x for x in input().upper() if ord('A') <= ord(x) <= ord('Z'))).values(), reverse=True)

for i, f in enumerate(count):
    n += f
    total += COSTS[i] * f
total += (n-1) * 3

print(total)
