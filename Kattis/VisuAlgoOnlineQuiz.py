
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
