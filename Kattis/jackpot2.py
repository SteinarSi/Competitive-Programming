n, m, f = input().split()
n = int(n)
m = float(m)
f = float(f)

def pr(d):
    return max(0, (m - (d*f)**2) / (n-d))

lo = 0
hi = n-1

while lo < hi:
    mi1 = (lo+hi) // 2
    mi2 = mi1+1
    pr1 = pr(mi1)
    pr2 = pr(mi2)

    if pr1 > pr2:
        hi = mi1
    elif pr1 < pr2:
        lo = mi2
    else:
        break

print(max(pr(lo), pr(hi)))
