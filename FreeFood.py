totalt = set()
for i in range(int(input())):
    inp = input()
    dager = inp.split(" ")
    for j in range(int(dager[0]), int(dager[1])+1):
        totalt.add(j)
    
print(len(totalt))