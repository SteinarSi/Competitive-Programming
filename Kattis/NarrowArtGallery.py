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

optleft  = [[0 for _ in range(n)] for _ in range(k+1)] # Du kan stenge, men kun left
optright = [[0 for _ in range(n)] for _ in range(k+1)] # Du kan stenge, men kun right

optright[0][0] = left[0] + right[0]
optleft [0][0] = left[0] + right[0]
for i in range(1, n):
    v = max(optleft[0][i-1], optright[0][i-1]) + left[i] + right[i]
    optleft[0][i], optright[0][i] = v, v
optleft [1][0] = right[0]
optright[1][0] = left[0]
inf = 10**10
for j in range(2, k+1):
    optleft[j][0], optright[j][0] = -inf, -inf

for nn in range(1, n):
    for kk in range(1, k+1):
        optleft [kk][nn] = max(max(optleft[kk][nn-1], optright[kk][nn-1]) + left [nn] + right[nn],  # Stenger ingen
                           optleft [kk-1][nn-1] + right[nn])                                        # Stenger venstre
        optright[kk][nn] = max(max(optleft[kk][nn-1], optright[kk][nn-1]) + left [nn] + right[nn],  # Stenger ingen
                           optright[kk-1][nn-1] + left [nn])                                        # Stenger høyre

print(max(optleft[k][n-1], optright[k][n-1]))
