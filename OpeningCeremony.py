input()
towers = list(map(int, input().strip().split()))
towers.sort()

n = 0
b = 0
ln = len(towers)
for i in range(len(towers)):
    if towers[i] - b <= 0:
        continue
    if towers[i] - b < ln - i:
        b += 1
        n += 1
    else:
        n += 1
print(n)