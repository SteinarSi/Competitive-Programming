from functools import cache

@cache
def collatz(x):
    if x == 1:
        return 1
    if x & 1:
        return 1 + collatz(x+x+x+1)
    return 1 + collatz(x >> 1)

print(max(range(1,1000001), key=collatz))
