MOD = 10**9 + 7

n, w, h = (int(i) for i in input().split())
max_rem = min(n, w*h)

dp = [[0] * w for _ in range(max_rem+1)]

for remaining in range(max_rem+1):
    dp[remaining][-1] = min(remaining+1, h+1)

for pos in range(w-2,-1,-1):
    for remaining in range(max_rem+1):
        for used in range(min(remaining, h) + 1):
            dp[remaining][pos] = (dp[remaining][pos] + dp[remaining-used][pos+1]) % MOD

boring = 0
for height in range(h+1):
    if n >= height*w:
        boring += 1
    else: 
        break

print((dp[max_rem][0] - boring) % MOD)
