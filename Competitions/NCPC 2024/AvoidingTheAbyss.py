xs, ys = (int(i) for i in input().split())
xt, yt = (int(i) for i in input().split())
xp, yp = (int(i) for i in input().split())

inf = 1000000

def safe(x, y):
    safe = None
    if y <= yp:
        return (x, -inf), (0, -inf)
    elif x <= xp:
        return (-inf, y), (-inf, 0)
    elif y >= yp:
        return (x, inf), (0, inf)
    elif x >= xp:
        return (inf, y), (inf, 0)

(x1, y1), (x2, y2) = safe(xs, ys)
(x5, y5), (x4, y4) = safe(xt, yt)

points = [(x1, y1), (x2, y2)]

if x2 == x4 and abs(y2 - y4) > inf:
    points.append((inf, 0))
elif y2 == y4 and abs(x2 - x4) > inf:
    points.append((0, inf))
points.extend([(x4, y4), (x5, y5)])

print(len(points))
for x,y in points:
    print(x, y)