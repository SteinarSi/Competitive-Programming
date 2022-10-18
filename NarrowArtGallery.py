import sys

n, k = input().split()
n, k = int(n), int(k)
left, right = [], []
for i in range(n):
    l, r = input().split()
    left.append(int(l))
    right.append(int(r))
input() # 0, 0

if k == 0:
    print(sum(left) + sum(right))
    sys.exit()

optboth  = [[0 for _ in range(n)] for _ in range(k+1)] # Du kan stenge, begge
optleft  = [[0 for _ in range(n)] for _ in range(k+1)] # Du kan stenge, men kun left
optright = [[0 for _ in range(n)] for _ in range(k+1)] # Du kan stenge, men kun right

optboth [0][0] = left[0] + right[0]
optright[0][0] = left[0] + right[0]
optleft [0][0] = left[0] + right[0]
for i in range(1, n):
    v = optboth[0][i-1] + left[i] + right[i]
    optboth[0][i], optleft[0][i], optright[0][i] = v, v, v
optboth [1][0] = max(left[0], right[0])
optleft [1][0] = right[0]
optright[1][0] = left[0]
inf = 10**10
for j in range(2, k+1):
    optboth[j][0], optleft[j][0], optright[j][0] = -inf, -inf, -inf

for nn in range(1, n):
    for kk in range(1, k+1):
        optboth [kk][nn] = max(optboth [kk  ][nn-1] + left [nn] + right[nn],    # Stenger ingen
                               optright[kk-1][nn-1] + left [nn],                # Stenger høyre
                               optleft [kk-1][nn-1] + right[nn])                # Stenger venstre
        optleft [kk][nn] = max(optboth [kk  ][nn-1] + left [nn] + right[nn],    # Stenger ingen
                               optleft [kk-1][nn-1] + right[nn])                # Stenger venstre
        optright[kk][nn] = max(optboth [kk  ][nn-1] + left [nn] + right[nn],    # Stenger ingen
                               optright[kk-1][nn-1] + left [nn])                # Stenger høyre

print(optboth[k][n-1])
