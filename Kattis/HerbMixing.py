ret = 0
green, red = (int(i) for i in input().split())

both = min(green, red)
green -= both
ret += 10 * both

ret += 10 * (green // 3)
green %= 3

match green:
    case 1: ret += 1
    case 2: ret += 3

print(ret)
