p, t = (int(i) for i in input().split())
xs = list(input())

alive = 0
queue = []
for i in range(len(xs)):
    if xs[i] == 'H': alive += 1
    elif xs[i] == 'I': queue.append(i)

c = p
while c < t and alive:
    next = []
    for i in queue:
        for j in (i-1,i+1):
            if 0 <= j < len(xs) and xs[j] == 'H':
                xs[j] = 'I'
                next.append(j)
                alive -= 1
    queue = next
    c += p

if alive:
    print('CURED')
else:
    print('ALL INFECTED')