n = int(input())
if n % 2 == 1:
    print("impossible")
    exit()

xs = {}
people = []
pool_sum = 0
billiard_sum = 0
for i in range(n):
    (p,b) = (int(j) for j in input().split())
    pool_sum += p
    billiard_sum += b

    if (p,b) in xs:
        xs[(p,b)].append(i)
    else:
        xs[(p,b)] = [i]
    people.append((p,b))

teams = n // 2

if pool_sum % teams != 0 or billiard_sum % teams != 0:
    print("impossible")
    exit()

used = [False] * n

target_pool = pool_sum // teams
target_billiard = billiard_sum // teams

for i, (p, b) in enumerate(people):
    if used[i]:
        continue
    i2 = xs[(p,b)].pop()
    used[i2] = True
    target = (target_pool - p, target_billiard - b)
    if target in xs and len(xs[target]) > 0:
        j = xs[target].pop()
        used[j] = True

    else:
        print("impossible")
        exit()

print("possible")
