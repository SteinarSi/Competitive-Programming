n, t = (int(i) for i in input().split())
playlists = [list(input()) for _ in range(n)]
matrix = [[0] * 26 for _ in range(n)]
for l, xs in enumerate(playlists):
    for x in xs:
        matrix[l][ord(x) - ord("a")] += 1

for _ in range(t):
    query = input().split()
    if query[0] == "PLAY":
        l = int(query[1]) - 1
        print("YES" if all((matrix[l][ord(x) - ord("a")] for x in query[2])) else "NO")
    else:
        x, i, y = (int(i) - 1 for i in query[1:])
        s = playlists[x].pop(i)
        playlists[y].append(s)
        matrix[x][ord(s) - ord("a")] -= 1
        matrix[y][ord(s) - ord("a")] += 1
