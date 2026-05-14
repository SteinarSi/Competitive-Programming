n, m = (int(i) for i in input().split())

xs = [input() for _ in range(n)]
toys = [int(input()) for i in range(m)]

t = 0
while n > 1:
    e = toys[t] % n
    xs = xs[e+1:] + xs[:e]
    t = (t+1) % m
    n -= 1

print(xs[0])
