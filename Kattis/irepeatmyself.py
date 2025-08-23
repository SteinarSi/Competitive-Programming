def explain(s):
    n = len(s)
    for l in range(1, n+1):
        times = n // l
        rest = n % l
        if rest == 0 and s[:l] * times == s[:times*l]:
            return l
        elif s[:rest] == s[-rest:] and s[:l] * times == s[:times*l]:
            return l

n = int(input())
for _ in range(n):
    print(explain(input()))