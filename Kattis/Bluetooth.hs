main=getLine>>getContents>>=print.s t t t t.lines
t=[1..8]
s l λ r ρ []
 | p l*p λ>0=0
 | p r*p ρ>0=1
 | True=2
s l λ r ρ ((a:_:_:'b':_):xs) 
 | a=='+'||a=='-'=s[][] r ρ xs
 | True = s l λ[][] xs
s l λ r ρ ((a:b:_):xs)
 | a=='+'=s(f b l) λ r ρ xs
 | a=='-'=s l(f b λ) r ρ xs
 | b=='+'=s l λ(f a r) ρ xs
 | b=='-'=s l λ r(f a ρ) xs
f c=filter (\x->read[c]/=x)
p=length