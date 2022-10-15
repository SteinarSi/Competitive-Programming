dager = input()
inp = input().split(" ")
total = 0
for i in range(int(dager)):
    if(int(inp[i]) < 0):
        total += 1
print(total)