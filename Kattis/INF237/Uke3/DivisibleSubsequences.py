I=input
for _ in range(int(I())):
 d=int(I().split()[0]);s,m=0,[0]*d
 for i in I().split():s+=int(i);m[s%d]+=1
 print(m[0]*(m[0]+1)//2+sum(n*(n-1)//2 for n in m[1:]))