n = int(input())
ret = [0] * n
graph = [[] for _ in range(n)]
for _ in range(n-1):
    u, v = (int(i) for i in input().split())
    graph[u].append(v)
    graph[v].append(u)

queue = [(0,0,0)]
while len(queue):
    (u,p,d) = queue.pop()
    ret[d] += 1
    for v in graph[u]:
        if v != p:
            queue.append((v, u, d+1))

print(max(enumerate(ret), key=lambda x: x[1])[0])
