def battle(d1,d2,d3):
    win1 = False
    win2 = False
    wins1 = 0
    wins2 = 0
    for s1 in d1:
        for s2 in d2:
            if s1 > s2:
                wins1 += 1
            elif s2 > s1:
                wins2 += 1
    
    if wins1 >= wins2 and wins1 > 0:
        win1 = True

    wins1 = 0
    wins2 = 0

    for s1 in d1:
        for s2 in d3:
            if s1 > s2:
                wins1 += 1
            elif s2 > s1:
                wins2 += 1
    
    if wins1 >= wins2 and wins1 > 0:
        win2 = True
    return win1 and win2

def main():
    d1 = []
    d2 = []
    d3 = []

    d1 = list(map(int, input().split()))
    d2 = list(map(int, input().split()))
    d3 = list(map(int, input().split()))
    
    if battle(d1,d2,d3):
        print(1)
    elif battle(d2,d1,d3):
        print(2)
    elif battle(d3,d2,d1):
        print(3)
    else:
        print('No dice')
main()