n, m, k = (int(i) for i in input().split())

graph = [[] for _ in range(n)]
for _ in range(m):
    u, v = (int(i)-1 for i in input().split())
    graph[u].append(v)
    graph[v].append(u)

games = [None] * n

def search(s):
    g = 1
    curr = [s]
    games[s] = g
    while len(curr):
        next = []
        for u in curr:
            for v in graph[u]:
                if games[v] is None:
                    games[v] = min(k,(g+1))
                    next.append(v)
                    g += 1
        curr = next
    return g >= k

for u in range(n):
    if games[u] is None:
        if not search(u):
            print('impossible')
            exit()

print(*games)
