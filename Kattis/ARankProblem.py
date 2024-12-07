n, m = (int(i) for i in input().split())

order = ['T' + str(i) for i in range(1, n+1)]

for _ in range(m):
    x, y = input().split()
    i, j = order.index(x), order.index(y)
    if i > j:
        order.remove(y)
        order.insert(i, y)

print(" ".join(order))
