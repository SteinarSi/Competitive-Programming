import fileinput

lines = list(fileinput.input())
i = 0

while i < len(lines):
    domain = lines[i].split()[1:]
    i += 1
    codomain = lines[i].split()[1:]
    i += 1
    n = int(lines[i])
    i += 1
    seen = set()
    mapped = set()    

    fun = True
    inj = True

    for j in range(i, i+n):
        x, _, y = lines[j].split()
        if x in seen:
            fun = False
        if y in mapped:
            inj = False
        
        seen.add(x)
        mapped.add(y)

    sur = len(mapped) == len(codomain)

    if not fun:
        print("not a function")
    elif inj and sur:
        print("bijective")
    elif inj:
        print("injective")
    elif sur:
        print("surjective")
    else:
        print("neither injective nor surjective")

    i += n
