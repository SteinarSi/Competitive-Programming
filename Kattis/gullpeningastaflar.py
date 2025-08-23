import sys

n = int(sys.stdin.readline())

query = '?'
for x in range(1,n+1):
    query += ' ' + str(x)
expected = n * (n*(n+1)) // 2

print(query, flush=True)
r = int(sys.stdin.readline())
print('!', r - expected, flush=True)
