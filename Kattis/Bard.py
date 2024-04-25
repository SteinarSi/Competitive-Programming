n = int(input())
e = int(input())

know = [set() for _ in range(n)]
songs = 0

for _ in range(e):
    xs = [int(i)-1 for i in input().split()][1:]

    if 0 in xs:
        for p in xs:
            know[p].add(songs)
        songs += 1
    else:
        sung = set()
        for p in xs:
            sung = sung.union(know[p])
        for s in sung:
            for p in xs:
                know[p].add(s)
        
for i in range(1, n+1):
    if len(know[i-1]) == songs:
        print(i)
