n, p = (int(i) for i in input().split())

spaceLeft = n - p
spaceLeft -= 2 * p * (spaceLeft // (2*p))

if p % 2 == 0 and spaceLeft == p:
    spaceLeft = 0

print(spaceLeft)
