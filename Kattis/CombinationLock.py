FULL = 40

while True:
    start, first, second, third = (int(i) for i in input().split())
    if (start, first, second, third) == (0,0,0,0):
        break
    x = 0
    x += 2 * FULL
    x += (start - first) % FULL
    x += FULL
    x += (second - first) % FULL
    x += (second - third) % FULL

    print(x * 9)
