import functools
import sys

sys.setrecursionlimit(1000000)

n, c = (int(i) for i in input().split())
xs = [int(i) for i in input().split()]

@functools.cache
def dp(k,i,b):
    if k == 0 or i >= n: return 0
    if xs[i] > k: return dp(k,i+1,b)
    if b: return 1 + dp(k-xs[i],i+1,True)
    else: return max(1 + dp(k-xs[i],i+1,True), dp(k,i+1,b))

print(dp(c,0,False))
