from functools import cache

@cache
def steps(s,ds,dd,d):
    if s == 0 and ds == 0 and dd == 0 and d >= 0:
        return 1
    if s <= 0 or abs(ds) > s or abs(dd) > 2*s or -(2*d) > s:
        return 0
    r = -1 if (ds+dd) & 1 else 1
    return steps(s-1,ds+r,dd,d-1) + steps(s-2,ds,dd+r,d+1)

for _ in range(int(input())):
    k, s = input().split()
    print(k, steps(int(s),0,0,0))
