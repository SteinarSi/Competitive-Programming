P=print
def q(u):
 for w in g[u+n]:
  if not r[w]:r[w]=1;q(w)
def d(u):
 v[u],s[u]=1,1
 for w in g[u]:
  if s[w]:P('inf');exit()
  if not v[w]and r[w]:d(w)
 p.append(u);s[u]=0
import sys
n=10000
sys.setrecursionlimit(n)
input()
g,r,p,v,s,o=[[]for _ in range(n*2)],[0]*n,[],[0]*n,[0]*n,[0,1]+[0]*n
for l in sys.stdin:f,t=map(int,l.split());g[f-1]+=[t-1];g[t+n-1]+=[f-1]
q(1)
d(0)
for u in p:
 for w in g[u]:o[u]+=o[w]
P(str(o[0])[-9:])