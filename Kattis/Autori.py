# Skrevet 30.08.19

fullenavn = input()

lengde = len(fullenavn)
i = 0
y = 1
navn = ''
Z = chr

while y <= lengde:
    if ord(fullenavn[i:y]) <= ord('Z') and ord(fullenavn[i:y]) >= ord('A'):
        navn += (fullenavn[i:y])
    i += 1
    y += 1
    
print(navn)