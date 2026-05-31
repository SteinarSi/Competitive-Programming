from queue import PriorityQueue

n = int(input())
costs = [int(i) for i in input().split()]

dependencies = []
dependents = [[] for _ in range(n)]

for u in range(n):
    xs = [int(i) for i in input().split()]
    dependencies.append(xs[0])
    for v in xs[1:]:
        dependents[v-1].append(u)

sources = [u for u in range(n) if dependencies[u] == 0]

def time_without(skip):
    queue = PriorityQueue()
    time = 0
    for s in sources:
        queue.put((costs[s] if s != skip else 0,s))
    done = [0] * n

    while not queue.empty():
        (t,u) = queue.get()
        time = t
        for v in dependents[u]:
            done[v] += 1
            if dependencies[v] == done[v]:
                if v == skip:
                    queue.put((time,v))
                else:
                    queue.put((time+costs[v],v))
    return time
times = min([time_without(s) for s in range(n)])
print(times)
