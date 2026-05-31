n, k = (int(i) for i in input().split())
xs = input()

ret = ""
p = 0
while n:
    p = (p + k - 1) % n 
    ret += xs[p]
    xs = xs[:p] + xs[p+1:]
    n -= 1

print(ret)
