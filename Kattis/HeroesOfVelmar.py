S=sum
u="Player 1"
v=u[:7]+'2'
l=[]
for c in 4,4,9,9,4,4:
 i=input().split()
 l.append(S({'Th':12 if (n:=int(i[0]))==4 else 6,'Se':n,'Ze':c,'Sh':6,'Ga':5,'Ra':4,'An':7,'Gu':8,'Fr':2}.get(h[:2],3)for h in i[1:]))
p=l[::2]
q=l[1::2]
s=S((i>j)-(i<j)for i,j in zip(p,q))
print(u if s>0 else v if s<0 else u if S(p)>S(q) else v if S(p)<S(q) else "Tie")