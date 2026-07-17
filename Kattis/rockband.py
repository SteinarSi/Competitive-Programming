m, s = (int(i) for i in input().split())
prefs = [[int(i) for i in input().split()] for _ in range(m)]

count = [0] * (s + 1)
errs = 0

for i in range(s):
    for b in range(m):
        x = prefs[b][i]
        count[x] += 1
        if count[x] == 1:
            errs += 1
        if count[x] == m:
            errs -= 1
    if errs == 0:
        print(i + 1)
        print(*filter(lambda x: count[x] == m, range(1, s + 1)))
        break
