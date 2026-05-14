n = int(input())
xs = [int(x) for x in input().split()]
xs.sort()
if n & 1:
    print(xs[n // 2])
else:
    s = xs[n // 2] + xs[n // 2 - 1]
    if s & 1:
        print(s / 2)
    else:
        print(s // 2)
