regions = {}
stars = []
rows = [0] * 10
cols = [0] * 10
regs = [0] * 10

for r in range(10):
    row = input()
    for c in range(10):
        regions[(r,c)] = int(row[c])

for r in range(10):
    row = input()
    for c in range(10):
        if row[c] == "*":
            stars.append((r, c))
            rows[r] += 1
            cols[c] += 1
            regs[regions[(r, c)]] += 1

if any(any(val != 2 for val in l) for l in (rows, cols, regs)):
    print("invalid")
else:
    for r, c in stars:
        for r2, c2 in stars:
            if (abs(r - r2) == 1 and abs(c - c2) == 1) or (abs(r - r2) == 1 and c == c2) or (abs(c - c2) == 1 and r == r2):
                print("invalid")
                exit()
    print("valid")
