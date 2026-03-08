while ((nps := input().strip()) != "0"):
    n, p, s = (int(i) for i in nps.split())
    dlls = [int(i) for i in input().split()]
    programs = [None]
    for _ in range(p):
        use, ds = input().split()
        programs.append((int(use), [ord(d) - ord('A') for d in ds]))

    usage = [0] * n
    current = 0
    ret = current
    for u in (int(i) for i in input().split()):
        if u > 0:
            use, ds = programs[u]
            current += use
            for d in ds:
                if usage[d] == 0: current += dlls[d]
                usage[d] += 1
            ret = max(ret, current)
        else:
            use, ds = programs[-u]
            current -= use
            for d in ds:
                usage[d] -= 1
                if usage[d] == 0: current -= dlls[d]
    print(ret)
