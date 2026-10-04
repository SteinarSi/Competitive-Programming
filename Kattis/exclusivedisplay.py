def combine(d1, d2):
    ret = ""
    for x, y in zip(d1, d2):
        if x == y:
            ret += "."
        else:
            ret += "#"
    return ret


digits = [
    """
.##.
#..#
#..#
....
#..#
#..#
.##.
""",
    """
....
...#
...#
....
...#
...#
....
""",
    """
.##.
...#
...#
.##.
#...
#...
.##.
""",
    """
.##.
...#
...#
.##.
...#
...#
.##.
""",
    """
....
#..#
#..#
.##.
...#
...#
....
""",
    """
.##.
#...
#...
.##.
...#
...#
.##.
""",
    """
.##.
#...
#...
.##.
#..#
#..#
.##.
""",
    """
.##.
...#
...#
....
...#
...#
....
""",
    """
.##.
#..#
#..#
.##.
#..#
#..#
.##.
""",
    """
.##.
#..#
#..#
.##.
...#
...#
.##.
""",
]
digits = ["".join(d.split()) for d in digits]

xs = "".join([input().strip() for _ in range(7)])

ret = []
for d, digit in enumerate(digits):
    if digit == xs:
        ret.append(d)

for tens in range(1, 10):
    for ones in range(0, 10):
        if xs == combine(digits[tens], digits[ones]):
            ret.append(10 * tens + ones)

if len(ret) == 0:
    print("impossible")
else:
    print(*ret)
