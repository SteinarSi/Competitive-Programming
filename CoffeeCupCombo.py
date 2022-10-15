input()
cups = 0
lectures = 0
for l in input():
    if l == '0':
        if cups > 0:
            lectures += 1
            cups -= 1
    else:
        cups = 2
        lectures += 1

print(lectures)