from math import floor

p, d = (int(i) for i in input().split())

districts_a = [0] * d
districts_b = [0] * d

for i in range(p):
    dis, a, b = (int(i) for i in input().split())
    districts_a[dis-1] += a
    districts_b[dis-1] += b

wasted_a = 0
wasted_b = 0
v = sum(districts_a) + sum(districts_b)

for i in range(d):
    nec = floor((districts_a[i] + districts_b[i]) / 2) + 1
    if districts_a[i] > districts_b[i]:
        wasted_a += districts_a[i] - nec
        wasted_b += districts_b[i]
        print(f"A {districts_a[i] - nec} {districts_b[i]}")
    else:
        print(f"B {districts_a[i]} {districts_b[i] - nec}")
        wasted_a += districts_a[i]
        wasted_b += districts_b[i] - nec

print(abs(wasted_a - wasted_b) / v)