


data Material = Rock | Air

type Chamber = [[Material]]


(-:) :: [Material] -> [[Material]] -> [[Material]]
(-:) [a,b,c,d,e,f,g] [as,bs,cs,ds,es,fs,gs] = [a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs]