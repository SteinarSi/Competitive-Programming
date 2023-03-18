from math import sqrt
d, u = (int(i) for i in input().split())
best = d
for p in range(2, round(sqrt(d))+1):
    if d % p == 0:
        if p >= u:
            best = min(p, best)
        if d // p >= u:
            best = min(d//p, best)

print(best)