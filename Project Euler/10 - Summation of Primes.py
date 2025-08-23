from math import sqrt, ceil

primes = [2]
x = 3
ret = 1
while x <= 2000000:
    xs = ceil(sqrt(x))
    for p in primes:
        if x % p == 0:
            break
        if p >= xs:
            primes.append(x)
            break
    x += 2
print(sum(primes))
