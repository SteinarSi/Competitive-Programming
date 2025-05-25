n = int(input())
curr = -1
m = 1
while True:
    curr += m
    if n <= curr:
        print("NO")
        break
    curr += m
    if n <= curr:
        print("YES")
        break
    m += 1
