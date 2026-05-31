n, m, k = (int(i) for i in input().split())
lit = [int(input())-1 for _ in range(n)]
graph = [[] for _ in range(n)]

for _ in range(m):
    u, v = (int(i)-1 for i in input().split())
    graph[u].append(v)

ret = 0
for u in range(n):
    s = k - (lit[u] % k) - 1
    ret += s
    for v in graph[u]:
        lit[v] += s

print(ret)
