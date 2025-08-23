import fileinput
from math import sqrt

def dist(ab, xy):
    (a,b), (x,y) = ab, xy
    return sqrt(abs(a-x)**2 + abs(b-y)**2)

gx, gy, dx, dy = (float(i) for i in input().split())

for line in fileinput.input():
    x,y = (float(i) for i in line.split())
    if dist((gx,gy),(x,y)) * 2 <= dist((dx,dy),(x,y)):
        print(f"The gopher can escape through the hole at ({x:.3f},{y:.3f}).")
        exit()

print("The gopher cannot escape.")
