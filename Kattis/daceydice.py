from queue import deque

# y, x, fw, up, lf
def forward(die):
    y,x,fw,up,lf = die
    return (y-1,x,up,7-fw,lf)

def backward(die):
    y,x,fw,up,lf = die
    return (y+1,x,7-up,fw,lf)

def left(die):
    y,x,fw,up,lf = die
    return (y,x-1,fw,7-lf,up)
def right(die):
    y,x,fw,up,lf = die
    return (y,x+1,fw,lf,7-up)

def hash(die):
    y,x,fw,up,lf = die
    return (y,x,fw,up)

for _ in range(int(input())):
    n = int(input())
    xs = [input() for _ in range(n)]

    for y in range(n):
        for x in range(n):
            if xs[y][x] == 'S':
                start = (y,x,4,1,5)
            elif xs[y][x] == 'H':
                end = (y,x)

    queue = deque()
    queue.append(start)
    seen = { start }

    while len(queue):
        die = queue.pop()
        y, x = die[0], die[1]
        for yy,xx,d in ((y-1,x,forward(die)), (y+1,x,backward(die)), (y,x+1,right(die)), (y,x-1,left(die))):
            if yy >= 0 and xx >= 0 and yy < n and xx < n and xs[yy][xx] != '*' and hash(d) not in seen:
                seen.add(hash(d))
                queue.append(d)
    
    y,x = end
    if any( (y,x,fw,2) in seen for fw in (1,3,4,6)):
        print("Yes")
    else:
        print("No")
