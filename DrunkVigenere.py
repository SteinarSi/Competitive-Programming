import string

alfa = list(string.ascii_uppercase)

cyph = input()
shift = input()

ret = ""

i = 0
while i < len(cyph):
    if i % 2 == 0:
        newindex = alfa.index(cyph[i]) - alfa.index(shift[i])
    else: 
        newindex = alfa.index(cyph[i]) + alfa.index(shift[i])

    newindex %= 26

    ret += alfa[newindex]

    i+=1

print(ret)