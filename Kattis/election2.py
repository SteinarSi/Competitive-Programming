I,d,f=input,{},{}
for _ in range(int(I())):
 x,y=I(),I()
 d[x]=y
l=[x for _ in range(int(I()))if (x:=I())in d]
for e in l:
 f[e]=l.count(e)
k=list(f.values())
f={y:x for x,y in f.items()}
print('tie'if k==[]or k.count(max(k))!=1 else d[f[max(k)]])