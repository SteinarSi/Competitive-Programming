n = int(input())
x = int(input())
y = int(input())
lights = sorted([x * int(i) for i in input().split()])

total = 0
ret = 0
for light in lights:
    if (total+light) / (ret+1) > y: break
    total += light
    ret += 1

print(ret)
