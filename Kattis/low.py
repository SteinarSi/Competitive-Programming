n,k = (int(x) for x in input().split())
xs = sorted([int(x) for x in input().split()])
m = 2*n*k

def feasable(d):
    i = 0
    a = 0
    while i < m-1:
        if xs[i+1] - xs[i] <= d:
            a += 2*(k-1)
            i += 2
        elif a >= 1:
            a -= 1
            i += 1
        else:
            return False
    return True

lo = 0
hi = xs[-1] - xs[0]

while lo < hi:
    d = lo + (hi-lo) // 2
    if feasable(d):
        hi = d
    else:
        lo = d + 1

print(lo)
