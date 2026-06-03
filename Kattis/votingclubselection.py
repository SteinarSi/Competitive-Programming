n = int(input())
c = int(input())
input()

ballots = [[(x[0], int(x[1:])) for x in input().split()] for _ in range(n)]

def count(bs):
    ret = {}
    for b in bs:
        for x,y in b:
            ret[x] = ret.get(x,0) + y
    return ret

def redistribute(loser,ballot):
    lost = sum(y for x,y in ballot if x == loser)
    ballot = [(x,y) for x,y in ballot if x != loser]
    most = -1
    winners = 0
    for x,y in ballot:
        if y > most:
            most = y
            winners = 1
        elif y == most:
            winners += 1
    return [(x,y if y < most else y + lost / winners) for x,y in ballot]

while c > 1:
    tally = count(ballots)
    least = min(tally.values())
    losers = [x for x,y in tally.items() if y == least]
    if len(losers) == c:
        print('VOID')
        exit()
    for loser in losers:
        ballots = [redistribute(loser,ballot) for ballot in ballots]
    c -= len(losers)

print(ballots[0][0][0])
