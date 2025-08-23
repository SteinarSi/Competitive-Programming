r, c = (int(i) for i in input().split())
map = [input() for _ in range(r)]

answers = [0] * 5

for y in range(r-1):
    for x in range(c-1):
        m = map[y][x:x+2] + map[y+1][x:x+2]
        if '#' in m:
            continue
        else: 
            answers[m.count('X')] += 1

for a in answers:
    print(a)