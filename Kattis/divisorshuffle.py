n = int(input())
xs = [int(i) for i in input().split()]

seen = set()
doubles = []
b = xs[0]
for x in xs:
    b = max(b, x)
    if x in seen:
        doubles.append(x)
    else:
        seen.add(x)

a = 1
for x in xs:
    if b % x != 0:
        a = max(a, x)
if a == 1:
    a = max(doubles)
print(a,b)
