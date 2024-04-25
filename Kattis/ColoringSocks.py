s, c, k = (int(i) for i in input().split())
xs = sorted(int(i) for i in input().split())

ret = 1
start = xs[0]
count = 1

for i in range(1,len(xs)):
    if count == c:
        ret += 1
        start = xs[i]
        count = 1
    elif xs[i] - start > k:
        ret += 1
        start = xs[i]
        count = 1
    else:
        count += 1

print(ret)
