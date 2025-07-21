from sys import setrecursionlimit
from functools import cache

setrecursionlimit(10000000)

MOD = 10**9+7
n, m = (int(i) for i in input().split())
d = m-n

@cache
def dp(b,g):
    return (int(g == 0 or g <= d) if b == 0
        else 0 if g == 0
        else (dp(b-1,g-1) + dp(b,g-1)) % MOD if b < g
        else (dp(b-1,g) + dp(b-1,g-1)) % MOD if b > g
        else 1
    )

print(dp(n,m))
