from functools import cache

n, c1, c2, c3 = (int(i) for i in input().split())
xs = [int(i) for i in input().split()]

m = sum(xs)
s = 0
yes = [True] * (m+4)
for x in xs:
    yes[s] = False
    s += x

@cache
def dp(i,a,b,c):
    return (i == m 
        or (i+1 <= m and yes[i+1] and a and dp(i+1,a-1,b,c))
        or (i+2 <= m and yes[i+2] and b and dp(i+2,a,b-1,c))
        or (i+3 <= m and yes[i+3] and c and dp(i+3,a,b,c-1))
    )

print("YES" if dp(0,c1,c2,c3) else "NO")
