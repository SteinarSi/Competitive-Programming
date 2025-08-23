a = input()
b = input()

xf = float(a)
yf = float(b)

xt = tuple((int(i) for i in a.split('.')))
yt = tuple((int(i) for i in b.split('.')))

if xf > yf and xt > yt:
    print(xf)
elif yf > xf and yt > xt:
    print(yf)
else:
    print(-1)