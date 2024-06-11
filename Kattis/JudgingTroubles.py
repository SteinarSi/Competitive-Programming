from collections import defaultdict

n = int(input())
dom = defaultdict(int)
ret = 0
for _ in range(n):
    dom[input()] += 1
for _ in range(n):
    inn = input()
    if dom[inn] > 0:
        ret += 1
        dom[inn] -= 1
print(ret)
