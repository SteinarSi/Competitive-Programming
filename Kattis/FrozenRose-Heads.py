
def cost(u, edges, visited, costs, cut_cost):
    visited[u] = True
    sum_costs = 0
    has_children = False
    for v in edges[u]:
        if not visited[v]:
            sum_costs += cost(v, edges, visited, costs, costs[u][v])
            has_children = True
    if not has_children:
        sum_costs = 10**10
    return min(cut_cost, sum_costs)

while True:
    inn = input()
    if inn == "": break
    n, r = inn.split()
    n, r = int(n), int(r)-1
    edges = [ [] for _ in range(n) ]
    costs = [ [0 for _ in range(n)] for _ in range(n) ]
    visited = [False for _ in range(n)]
    for _ in range(n-1):
        u, v, c = input().split()
        u, v, c = int(u)-1, int(v)-1, int(c)
        costs[u][v], costs[v][u] = c, c
        edges[u].append(v)
        edges[v].append(u)
    print(cost(r, edges, visited, costs, 10**10))

'''
2 1
1 2 4
4 4
1 2 1
2 3 1
4 2 3
5 5
5 2 3
1 5 3
5 3 2
4 3 1
'''