def q(u):
 for w in b[u]:
  if not r[w]:r[w]=1;q(w)
def d(u):
 v[u],s[u]=1,1
 for w in g[u]:
  if s[w]:print('inf');exit()
  if not v[w]and r[w]:d(w)
 p.append(u);s[u]=0
import sys
n=10000
sys.setrecursionlimit(n)
input()
g,b,r,p,v,s,o=[[]for _ in range(n)],[[]for _ in range(n)],[0]*n,[],[0]*n,[0]*n,[0]*n
for l in sys.stdin:f,t=(int(i)-1 for i in l.split());g[f].append(t);b[t].append(f)
q(1)
d(0)
o[1]=1
for u in p:
 for w in g[u]:o[u]+=o[w]
print(str(o[0])[-9:])