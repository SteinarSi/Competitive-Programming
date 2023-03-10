inns = []

n = int(input())
grid = [[0 for _ in range(n+1)] for _ in range(n+1)]
grid[1][1] = 1
for y in range(1, n+1):
    inn = input()
    inns.append(inn)
    for x in range(1, n+1):
        if y == 1 and x == 1: 
            continue
        innnn = inn[x-1]
        grid[y][x] = 0 if innnn == '#' else grid[y-1][x] + grid[y][x-1]

if grid[n][n] != 0:
    print(grid[n][n] % (2**31 - 1))
else:
    #visited = set()

    visited = [[False for _ in range(n)] for _ in range(n)]

    to_search = {(0, 0)}
    while len(to_search) > 0:
        x, y = to_search.pop()
        #visited.add((x, y))
        if x > 0     and inns[y][x-1] != '#' and not visited[y][x-1]:
            to_search.add((x-1, y))
            visited[y][x-1] = True
        if y > 0     and inns[y-1][x] != '#' and not visited[y-1][x]:
            to_search.add((x, y-1))
            visited[y-1][x] = True
        if x + 1 < n and inns[y][x+1] != '#' and not visited[y][x+1]:
            to_search.add((x+1, y))
            visited[y][x+1] = True
        if y + 1 < n and inns[y+1][x] != '#' and not visited[y+1][x]:
            to_search.add((x, y+1))
            visited[y+1][x] = True
        #print(to_search)

    if visited[n-1][n-1]:
        print("THE GAME IS A LIE")
    else:
        print('INCONCEIVABLE')