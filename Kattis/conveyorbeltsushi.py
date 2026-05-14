m = int(input())
menu = [int(i) for i in input().split()]
n, e = (int(i) for i in input().split())
events = [tuple((int(i) for i in input().split())) for _ in range(e)]
bill = [0] * n
belt = [None] * (n+1)
chef = 0
offset = 0
t = 0
i = 0

while i < len(events):
    offset = t % (n+1)

    if belt[-offset] is None:
        belt[-offset] = menu[chef]
        chef = (chef+1) % m

    while i < len(events) and events[i][0] == t:
        bill[events[i][1]-1] += belt[events[i][1] - offset]
        belt[events[i][1] - offset] = None
        i += 1
    t += 1

for b in bill:
    print(b)
