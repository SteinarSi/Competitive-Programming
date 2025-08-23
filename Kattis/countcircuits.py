from functools import cache

n = int(input())
xs = [[int(x) for x in input().split()] for _ in range(n)]

@cache
def dp(i,x,y):
    if i == n: return x==y==0
    dx,dy = xs[i]
    return dp(i+1,x,y) + dp(i+1,x+dx,y+dy)

print(dp(0,0,0) - 1)
