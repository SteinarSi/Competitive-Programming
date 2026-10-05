sx, sy = (int(i) for i in input().split())
tx, ty = (int(i) for i in input().split())

dx = abs(sx - tx)
dy = abs(sy - ty)

if sx == tx:
    if sy < ty:
        print("N")
    else:
        print("S")
elif sy == ty:
    if sx < tx:
        print("E")
    else:
        print("W")
else:
    if sx < tx and sy < ty:
        print("NE")
        if dy > dx:
            print("N")
        elif dy < dx:
            print("E")

    elif sx < tx and sy > ty:
        print("SE")
        if dy > dx:
            print("S")
        elif dy < dx:
            print("E")

    elif sx > tx and sy < ty:
        print("NW")
        if dy > dx:
            print("N")
        elif dy < dx:
            print("W")

    elif sx > tx and sy > ty:
        print("SW")
        if dy > dx:
            print("S")
        elif dy < dx:
            print("W")
