xs = []
ys = []
while (x := input()) != "q":
    xs.append(x)
    ys.append(tuple(x.split()))

print(xs)
print(ys)
