n, m = (int(i) for i in input().split())
n+=1
graph = [[] for _ in range(n)]
for _ in range(m):
    u, v = (int(i) for i in input().split())
    graph[u].append(v)
a, b = (int(i) for i in input().split())

def reachable(s):
    ret = [False]*n
    ret[s] = True
    queue = [s]
    while queue:
        u = queue.pop()
        for v in graph[u]:
            if not ret[v]:
                ret[v] = True
                queue.append(v)
    return ret

xs = reachable(a)
ys = reachable(b)
for u in range(1,n):
    if xs[u] and ys[u]:
        print('yes')
        print(u)
        exit()
print('no')
