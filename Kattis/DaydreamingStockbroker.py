values = [int(input()) for _ in range(int(input()))]

n = 0
c = 100
for i in range(len(values)-1):
    c = c + n * values[i]
    n = 0
    if values[i] <= values[i+1]:
        n = min(c // values[i], 100_000)
        c = c - n * values[i]
c = c + n * values[-1]

print(c)