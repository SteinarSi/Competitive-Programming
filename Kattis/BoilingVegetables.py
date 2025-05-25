from queue import PriorityQueue

T = float(input().split()[0])
q = PriorityQueue()
smallest = 10**10
for w in input().split():
    x = int(w)
    smallest = min(smallest, x)
    q.put((-x,x,1))

ret = 0
while True:
    largest,orig,num = q.get()
    if smallest / (-largest) >= T:
        break
    new = orig / (num + 1)
    smallest = min(smallest, new)
    q.put((-new,orig,num+1))
    ret += 1

print(ret)
