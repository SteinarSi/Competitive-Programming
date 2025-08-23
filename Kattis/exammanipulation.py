I=input
n,k=(int(i)for i in I().split())
a=[int(I().replace('T','1').replace('F','0'),2)for _ in range(n)]
print(max(min(sum(A&(1<<j)!=b&1<<j for j in range(k))for A in a)for b in range(2**k)))