n = int(input())
xs = list(zip([-9999999999] + [int(i) for i in input().split()] + [999999999999], range(0,n+2)))
xs.sort()

best = 0
best_spot = -1
for i in range(1,n+1):
    dist = min(xs[i][0]-xs[i-1][0], xs[i+1][0]-xs[i][0])
    if dist > best or dist == best and xs[i][1] < best_spot:
        best = dist
        best_spot = xs[i][1]

print(best_spot)
