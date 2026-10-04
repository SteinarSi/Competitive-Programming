n = int(input())
xs = [int(input()) for _ in range(3)]
xs.sort()

if xs[0] + n < xs[1]:
    print(xs[0] + n)
else:
    print("impossible")
