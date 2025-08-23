r, s, m, d, n = (int(i) for i in input().split())
brands = [None] + [int(i) for i in input().split()]
smd = s+m+d

dishes = [None]
for _ in range(smd):
    dishes.append([int(i) for i in input().split()][1:])

INF = 10**18

banned = [[False for _ in range(smd+1)] for _ in range(smd+1)]
for _ in range(n):
    i,j = (int(i) for i in input().split())
    banned[i][j] = True
    banned[j][i] = True

ret = 0
for starter in range(1,s+1):
    for main in range(s+1, s+m+1):
        if banned[starter][main]: continue
        for dessert in range(s+m+1,smd+1):
            if banned[starter][dessert]: continue
            if banned[main][dessert]: continue
            combs = 1
            used = set()
            for ingr in dishes[starter] + dishes[main] + dishes[dessert]:
                if ingr in used: continue
                used.add(ingr)
                combs *= brands[ingr]
            ret += combs

            if ret > INF:
                print("too many")
                exit()

print(ret)
