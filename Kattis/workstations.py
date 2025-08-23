from collections import deque

n, m = (int(i) for i in input().split())

events = []
for _ in range(n):
    a, s = (int(i) for i in input().split())
    events.append((a,1))
    events.append((a+s,0))
events.sort()

available = deque()
unlocks = 0
for a,s in events:
    if s == 0:
        available.append(a+m)
    else:
        while len(available) and available[0] < a:
            available.popleft()
        if len(available):
            available.popleft()
        else: 
            unlocks += 1

print(n - unlocks)
