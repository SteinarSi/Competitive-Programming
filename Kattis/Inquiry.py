ns = [int(input()) for _ in range(int(input()))]

squares = 0
linears = sum(ns)

sums = []

for i in range(len(ns)-1):
    squares += ns[i]**2
    linears -= ns[i]
    sums.append(squares * linears)

print(max(sums))