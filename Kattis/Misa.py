r, s = input().split()
r, s = int(r), int(s)

seats = [[0] + [1 if s == 'o' else 0 for s in input()] + [0] for _ in range(r)]
seats.insert(0, [0] * (s+2))
seats.append([0] * (s+2))

best = 0
summ = 0
for y in range(1, r+1):
    for x in range(1, s+1):
        if seats[y][x] == 1:
            summ += seats[y][x+1] + seats[y+1][x-1] + seats[y+1][x] + seats[y+1][x+1]
        else:
            best = max(best, sum([sum([ seats[yy][xx] for yy in range(y-1, y+2) if (yy, xx) != (y, x) ]) for xx in range(x-1, x+2)]))
        
print(best+summ)
        
        
