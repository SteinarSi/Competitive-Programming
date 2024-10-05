import sys
sys.set_int_max_str_digits(2001)

s = int(input(), 2)
d = int(input(), 2)
m = int(input(), 2)

money = m
days = 0

if m >> (d-1) == 0:
    while money > 0:
        days += 1
        money >>= 1
    print(bin(days)[2:])
    exit()

salaryTooLow = s >> (d-1) == 0
if s >> (d-1) > 0:
    print("Infinite money!")
    exit()

while money >> (d-1) > 0:
    days += d
    money = (money >> d) + s

    if money >= m:
        print("Infinite money!")
        exit()

while money > 0:
    money >>= 1
    days += 1

print(bin(days)[2:])
