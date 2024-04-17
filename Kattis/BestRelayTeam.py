from queue import PriorityQueue
from itertools import combinations

first = PriorityQueue()
second = PriorityQueue()

for _ in range(int(input())):
    name,f,s = input().split()
    f = float(f)
    s = float(s)
    first.put((f,(name,f,s)))
    second.put((s,(name,f,s)))

shortlist = set()
for _ in range(4):
    shortlist.add(first.get()[1])
    shortlist.add(second.get()[1])

best_time = 999999999999999
best_team = None

for (n1,f1,s1) in shortlist:
    for ((n2,_,s2),(n3,_,s3),(n4,_,s4)) in combinations(shortlist - {(n1,f1,s1)}, 3):
        time = f1 + s2 + s3 + s4
        if time < best_time:
            best_time = time
            best_team = (n1,n2,n3,n4)

print(best_time)
for n in best_team:
    print(n)
