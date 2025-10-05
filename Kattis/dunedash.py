from math import dist

n = int(input())
xs = [tuple((int(i) for i in input().split())) for _ in range(n)]

s = max(xs,key=lambda x: dist(x,xs[0]))
xs.sort(key=lambda x: dist(s,x))

ret = 0
for i in range(1,n):
    ret += dist(xs[i-1],xs[i])

print(ret)
