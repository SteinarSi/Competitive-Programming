for _ in range(int(input())):
 d=int(input().split()[0]);s,m=0,[0]*d
 for i in input().split():s+=int(i);m[s%d]+=1
 print(m[0]*(m[0]+1)//2+sum(n*(n-1)//2 for n in m[1:]))