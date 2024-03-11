def I():return[int(i)for i in input().split()]
g,n=I()
r,p=0,0
for e,s in sorted([tuple(reversed(I()))for _ in range(n)]):
 if s>=p:r+=1;p=e
print("YES"if r>=g else"NO")