# This is for the easy version of 99 problems, see 99Problems.cpp for the hard version

n = int(input())

if n <= 100:
    n = 99
elif n % 100 <= 48:
    n -= n % 100 + 1
else:
    n += 100 - n % 100 - 1

print(n)

