for _ in range(int(input())):
    xs = [int(i) for i in input().split()]
    print("%.3f" % (100 * sum(1 for x in xs[1:] if x > sum(xs[1:])/xs[0]) / xs[0]), end="%\n")