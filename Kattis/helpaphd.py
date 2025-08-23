for _ in range(int(input())):
    print('skipped' if (t := input()) == "P=NP" else eval(t))
