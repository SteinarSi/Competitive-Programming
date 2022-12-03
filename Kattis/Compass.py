a = int(input())
b = int(input())

right = abs(a-b)
left = 360 - right

if right < left:
    if b >= a:
        print(right)
    else:
        print(-right)
elif left < right:
    if b >= a:
        print(-left)
    else:
        print(left)
else:
    print(180)