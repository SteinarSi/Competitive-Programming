from math import ceil, sqrt

def power_prefix(s):
    bs = []
    a = 1
    n = len(s)
    r = ceil(sqrt(n))
    while a <= r:
        if n % a == 0:
            if s[-a:]*(n // a) == s: return n // a
            bs.append(n // a)
        a += 1
    bs.reverse()
    for b in bs:
        if s[-b:] * (n // b) == s: return n // b
    
while ((inn := input().strip()) != "."):
    print(power_prefix(inn))