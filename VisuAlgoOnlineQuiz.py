
from queue import PriorityQueue
import sys
sys.setrecursionlimit(10_001)

V, E = input().split()
V, E = int(V), int(E)

neighbours = [[] for _ in range(V)]

edges = []

for _ in range(E):
    u, v, w = input().split()
    u, v, w = int(u), int(v), int(w)
    edges.append((u, v))
    neighbours[u].append((v, w))

s, t = input().split()
s, t = int(s), int(t)

toT = [u for u, v in edges if v == t]

dist = [10**10 for _ in range(V)]
dist[s] = 0
visited = [False for _ in range(V)]

preorder = []
q = PriorityQueue()
q.put((0, s))
while not q.empty() and len(toT) > 0:
    w, u = q.get()
    if visited[u]:
        continue
    preorder.append(u)
    visited[u] = True
    if w > dist[t]:
        break
    for v, w in neighbours[u]:
        if v == t:
            toT.remove(u)
            if dist[u] + w <= dist[v]:
                dist[v] = dist[u] + w
        elif dist[u] + w < dist[v]:
            dist[v] = dist[u] + w
            q.put((dist[v], v))

seen = [0 for _ in range(V)]
seen[s] = 1
for u in preorder:
    for v, w in neighbours[u]:
        if dist[u] + w <= dist[v]:
            seen[v] += seen[u]

print(seen[t])

def dfs(u):
    if u == t:
        return 1
    summ = 0
    for v, w in neighbours[u]:
        if dist[u] + w <= dist[v]:
            summ += dfs(v)
    return summ

#print("dfs", dfs(s))


'''
4 3
0 1 1
1 2 2
2 0 3
0 3

'''



'''

visited = [False for _ in range(V)]
visited[s] = True

paths = []
best = [10**10]
memod = [False for _ in range(V)]
memo = [0 for _ in range(V)]

def dfs(u, c):
    if u == t:
        best[0] = min(c, best[0])
        paths.append(c)
        print(visited)
        return (c, 1)
    else:
        min_cost = 10**10
        n_paths = 0
        for (v, w) in neighbours[u]:
            if not visited[v] and c+w <= best[0]:
                if memod[v]:
                    (cc, pths) = memo[v]
                else:
                    visited[v] = True
                    (cc, pths) = dfs(v, c+w)
                    visited[v] = False
                if cc < min_cost:
                    min_cost = cc
                    n_paths = pths
                
        memo[u] = (min_cost, n_paths)
        memod[u] = True
        return (min_cost, n_paths)

dfs(s, 0)

if paths == []:
    print(0)
else:
    m = min(paths)
    print(sum([1 for c in paths if c == m]))

    '''