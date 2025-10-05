from heapq import heappush, heappop

n, k = (int(i) for i in input().split())

queue = []
for q, t in enumerate((int(i) for i in input().split())):
    heappush(queue, (t,q+1))

while len(queue):
    _, q = heappop(queue)
    print(q,flush=True)
    t = input()
    if t != 'DONE':
        heappush(queue, (int(t), q))

print('DONE')
