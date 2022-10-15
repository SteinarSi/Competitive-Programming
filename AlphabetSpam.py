# Skrevet 06.09.19

streng = input()
små = 0
store = 0
mellomrom = 0
andre = 0

for karakter in streng:
    if karakter == "_":
        mellomrom += 1
    elif karakter.islower():
        små += 1
    elif karakter.isupper():
        store += 1
    else:
        andre += 1

totalt = små + store + mellomrom + andre
print(mellomrom / totalt)
print(små / totalt)
print(store / totalt)
print(andre / totalt)