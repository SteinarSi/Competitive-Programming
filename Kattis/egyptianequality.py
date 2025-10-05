n = int(input())
pyramid = [input() for _ in range(n)]

cs = 0
for y in range(n):
    for x in range(2*n-1):
        if pyramid[y][x] == 'C':
            cs += 1
if cs & 1:
    print('impossible')
    exit()
goal = cs // 2

def bottom_up():
    ret = [list(ys) for ys in pyramid]
    a = 0
    for x in range(2*n-1):
        ret[-1][x] = 'A'
        if pyramid[-1][x] == 'C':
            a += 1
    if a > goal:
        return False
    ret[0][n-1] = 'B'

    for y in range(n-2,0,-1):
        for x in (n-1-y, n-1+y, *range(n-y,n-1+y)):
            if a < goal:
                ret[y][x] = 'A'
                if pyramid[y][x] == 'C': a += 1
            else:
                ret[y][x] = 'B'
    return ret

def left_snake():
    ret = [list(ys) for ys in pyramid]
    a = int(pyramid[0][n-1] == 'C')
    ret[0][n-1] = 'A'
    for y in range(1,n):
        if a == goal:
            ret[y][n-y-1] = 'B'
            ret[y][n-y] = 'B'
        else:
            for x in n-y-1, n-y:
                ret[y][x] = 'A'
                if pyramid[y][x] == 'C':
                    a += 1
            if a > goal:
                return False
    for y in range(1,n):
        for x in range(n-y+1,n+y):
            if a < goal:
                ret[y][x] = 'A'
                if pyramid[y][x] == 'C': a += 1
            else:
                ret[y][x] = 'B'
    return ret

def right_snake():
    ret = [list(ys) for ys in pyramid]
    a = int(pyramid[0][n-1] == 'C')
    ret[0][n-1] = 'A'
    for y in range(1,n):
        if a == goal:
            ret[y][n+y-1] = 'B'
            ret[y][n+y-2] = 'B'
        else:
            for x in n+y-1, n+y-2:
                ret[y][x] = 'A'
                if pyramid[y][x] == 'C':
                    a += 1
            if a > goal:
                return False
    for y in range(1,n):
        for x in range(n+y-3,n-y-2,-1):
            if a < goal:
                ret[y][x] = 'A'
                if pyramid[y][x] == 'C': a += 1
            else:
                ret[y][x] = 'B'
    return ret

for sol in bottom_up(), left_snake(), right_snake():
    if sol:
        for row in sol:
            print(''.join(row))
        exit()

print('impossible')
