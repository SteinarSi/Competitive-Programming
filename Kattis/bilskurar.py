def sort_and_count(L):
    n = len(L)
    if n <= 1: return 0, L
    inv1, L1 = sort_and_count(L[:n//2])
    inv2, L2 = sort_and_count(L[n//2:])
    count = inv1 + inv2
    L = []
    p1, p2 = 0, 0
    while p1 < len(L1) and p2 < len(L2):
        if L1[p1] < L2[p2]:
            L.append(L1[p1])
            p1 += 1
        else:
            L.append(L2[p2])
            p2 += 1
            count += len(L1) - p1 # <- this is where the magic happens
    L += L1[p1:]
    L += L2[p2:]
    return count, L

n = int(input())
xs = (int(i) for i in input().split())
ys = (int(i) for i in input().split())

index = [0] * (n+1)
for i, y in enumerate(ys):
    index[y] = i
mapped = [index[x] for x in xs]

print(sort_and_count(mapped)[0])
