s,n = [int(i) for i in input().split()]

xs = [False] * s
for x in [int(i)-1 for i in input().split()]:
    xs[x] = True

ret = 0
for i in range(s):
    if not (xs[i] or xs[(i+1) % s] or xs [(i-1) % s]):
        ret += 1
        xs[i] = True

print(ret)
