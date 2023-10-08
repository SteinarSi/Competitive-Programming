n, m = (int(i) for i in input().split())

items = sorted([int(x) for x in input().split()], reverse=True)

kids = items[:m]
for i in range(m, n):
    kids[m-i-1] += items[i]

print(max(kids))
