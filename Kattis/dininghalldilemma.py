n, k, d = (int(i) for i in input().split())

forced = sum(1 << (int(b) - 1) for b in input().split())
missing = d - forced

if (
    missing % 2 == 0
    and 0 <= missing <= (1 << (n + 1))
    and all(
        (missing & (1 << b)) == 0 or (b > 0 and (forced & (1 << (b - 1))) == 0)
        for b in range(0, n + 2)
    )
):
    print(1 << k)
else:
    print(0)
