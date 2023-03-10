# Skrevet 30.08.19

antall = int(input())


i = int(0)

while i < antall:
    a = int(input())
    sjekk = a%2
    if sjekk == 0:
        print(a, "is even")
    else:
        print(a, "is odd")
    i += 1