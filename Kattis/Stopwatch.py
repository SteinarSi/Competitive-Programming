sum = 0
for i in range(int(input())):
    if i % 2 == 0:
        start = int(input())
    else:
        sum += int(input()) - start
if i % 2 == 0:
    print("still running")
else:
    print(sum)