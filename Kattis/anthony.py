n, m = (int(i) for i in input().split())
xs = [float(input()) for _ in range(n+m-1)]

dp = [[0.0] * (m+1) for _ in range(n+1)]
for a in range(1,n+1):
    dp[a][0] = 1
    for c in range(1,m+1):
        p = xs[n+m-a-c]
        dp[a][c] = p * dp[a][c-1] + (1-p) * dp[a-1][c]

print(dp[n][m])
