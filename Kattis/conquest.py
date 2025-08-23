from queue import PriorityQueue as Q
I=input
n,m=(int(i)for i in I().split())
s=[1]+[0]*n
g=[[]for _ in range(n)]
for _ in range(m):
 f,t=(int(i)-1 for i in I().split())
 g[f]+=[t]
 g[t]+=[f]
S=[int(I())for _ in range(n)]
q=Q()
q.put((0,0))
r=S[0]
for v in g[0]:
 if not s[v]:q.put((S[v],v));s[v]=1
while not q.empty():
 w,u=q.get()
 if w>=r:break
 for v in g[u]:
  if not s[v]:q.put((S[v],v));s[v]=1
 r+=w
print(r)