w, h, d = (int(i) for i in input().split())

lines = [input() for _ in range(h)]

for y, l in enumerate(lines):
    if "@" in l:
        camp = (l.index("@"), y)
    if "J" in l:
        jaguar = (l.index("J"), y)


if (abs(camp[0] - jaguar[0])**2 + abs(jaguar[1] - camp[1])**2) <= d*d:
    print("the guide is right")
else:
    print("no jumpscares here")