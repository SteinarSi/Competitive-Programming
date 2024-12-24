def pow(x, n, m):
   if (n == 0): return 1
   u = pow(x,n//2,m)
   u = (u*u)%m
   if (n & 1): u = (u*x)%m
   return u

[a,e,m] = (int(i) for i in input().split())

print(pow(a,e,m))
