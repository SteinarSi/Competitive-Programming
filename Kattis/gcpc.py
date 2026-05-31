n, m = (int(i) for i in input().split())

better = {}
worse = {i: (0,0) for i in range(2,n+1)}

our_score = 0
our_penalty = 0

for _ in range(m):
    t, p = (int(i) for i in input().split())
    if t == 1:
        our_score += 1
        our_penalty += p
        for t, (sc,pe) in list(better.items()):
            if sc < our_score or sc == our_score and pe >= our_penalty:
                del better[t]
                worse[t] = (sc,pe)
    else:
        if t in worse:
            (sc,pe) = worse[t]
            del worse[t]
        if t in better:
            (sc,pe) = better[t]
            del better[t]
        sc += 1
        pe += p

        if sc > our_score or sc == our_score and pe < our_penalty:
            better[t] = (sc,pe)
        else:
            worse[t] = (sc,pe)
    print(1 + len(better))
