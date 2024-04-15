from queue import PriorityQueue

def diff(a, b):
    r = 0
    for i in range(len(a)):
        if a[i] != b[i]:
            r += 1
    return r

n, k = (int(i) for i in input().split())

dna = [input() for _ in range(n)]

seen = [False] * n
s = [0]
seen[0] = True
edges = []
q = PriorityQueue()
for v in range(1,n):
    q.put(((diff(dna[0],dna[v]), (0,v))))

ret = 0
while not q.empty():
    (d, (u,v)) = q.get()
    if not seen[v]:
        seen[v] = True
        ret += d
        edges.append((u,v))
        for w in range(n):
            if not seen[w]:
                q.put((diff(dna[v], dna[w]), (v,w)))
    if len(edges) == n - 1:
        break

print(ret)
for (u,v) in edges:
    print(u,v)
