import sys
from math import lcm

EARTH = 365
MARS = 687

def day_since(e, m):
    if e != 0:
        return e + day_since(0, (m - e) % MARS)
    elif m == 0:
        return 0
    else:
        return EARTH + day_since(0, (m-EARTH) % MARS)

for i, line in enumerate(sys.stdin):
    print(f"Case {i+1}: ", end="")
    earth, mars = (int(i) for i in line.split())
    if (earth,mars) == (0,0):
        print(0)
    else:
        print(lcm(EARTH,MARS) - day_since(earth, mars))
