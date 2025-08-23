MOD = 10**9

def multiply(a,b):
    return (
        (a[0] * b[0] + a[1] * b[2]) % MOD,
        (a[0] * b[1] + a[1] * b[3]) % MOD,
        (a[2] * b[0] + a[3] * b[2]) % MOD,
        (a[2] * b[1] + a[3] * b[3]) % MOD
    )

def power(m,e):
    ret = (1,0,0,1)
    while e:
        if e & 1:
            ret = multiply(ret,m)
        m = multiply(m,m)
        e >>= 1
    return ret

def fibonacci(n):
    return multiply(power((0,1,1,1),n-1),(1,0,1,0))[0]

for _ in range(int(input())):
    c, y = (int(i) for i in input().split())
    print(c, fibonacci(y))
