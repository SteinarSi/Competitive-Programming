n, m = (int(i) for i in input().split())

outgraph = [set() for _ in range(n+1)]
ingraph = [[] for _ in range(n+1)]

for _ in range(m):
    u, v = (int(i) for i in input().split())
    outgraph[u].add(v)
    ingraph[v].append(u)

best = 0
best_celeb = 1
for celeb in range(1, n+1):
    if len(ingraph[celeb]) <= best: continue

    cc = 0
    for v in ingraph[celeb]:
        if v not in outgraph[celeb]:
            cc += 1

    if cc > best:
        best = cc
        best_celeb = celeb

print(best_celeb, best)
