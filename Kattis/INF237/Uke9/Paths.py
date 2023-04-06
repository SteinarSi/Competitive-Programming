def dfs(u, opt, graph, color, visit_mask):
    if opt[u][visit_mask] != -1:
        return opt[u][visit_mask]
    sum = 0
    for v in graph[u]:
        if (1 << color[v]) & visit_mask: continue
        sum += dfs(v, opt, graph, color, visit_mask | (1 << color[v])) + 1
    opt[u][visit_mask] = sum
    return sum

n, m, k = (int(i) for i in input().split())
color = [int(i)-1 for i in input().split()]
graph = [[] for _ in range(n)]
for _ in range(m):
    a, b = (int(i)-1 for i in input().split())
    graph[a].append(b)
    graph[b].append(a)

opt = [[-1] * 2**k for _ in range(n)]
total = 0
for u in range(n):
    total += dfs(u, opt, graph, color, 1 << color[u])

print(total)