from queue import PriorityQueue
from math import floor, inf

n, m, f, s, t = [int(i) for i in input().split()]

graph = [[] for _ in range(n)]
for _ in range(m):
    i, j, c = [int(i) for i in input().split()]
    graph[i].append((j, c, False))
    graph[j].append((i, c, False))
for _ in range(f):
    i, j = [int(i) for i in input().split()]
    graph[i].append((j, 0, True))

dist = [inf] * n
dist[s] = 0

q = PriorityQueue()
q.put((0, False, s))

while not q.empty():
    d, used, u = q.get()
    for v, c, f in graph[u]:
        if f and used: continue
        if d + c < dist[v]:
            dist[v] = d + c
            q.put((dist[v], used or f, v))

print(dist[t])
