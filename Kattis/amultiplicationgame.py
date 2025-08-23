from sys import stdin
from collections import defaultdict

def maxi(p, n, memo):
    if memo[p] or p*9 >= n:
        return 1
    r = max(mini(p*i, n, memo) for i in range(2, 10))
    memo[p] = r == 1
    return r

def mini(p, n, memo):
    if memo[p] or p*9 >= n:
        return -1
    r = min(maxi(p*i,n, memo) for i in range(2, 10))
    memo[p] = r == -1
    return r

def game(n):
    return "Stan wins." if maxi(1, n, defaultdict(bool)) == 1  else "Ollie wins."

for n in stdin:
    print(game(int(n)))


'''
4294967294

def simon(n):
    p = 1
    curr = 0
    while p < n:
        curr ^= 1
        if curr == 0:
            p *= 2
        else:
            p *= 9
    if curr:
        return "Stan wins."
    else:
        return "Ollie wins."
'''
for i in range(1, 1000):
    a, b = simon(i), game(i)
    if a != b:
        print(i, a, b)
'''
wrong = [
    1,
    36, 
    37, 
    38,
    70,
    69,
    4294967294,
    879783732,
    2869567135,
    3852985759,
    544181336,
    1282366808,
    1064031966,
    1816427473,
    3432651658,
    120291096,
    1516587719,
    4170697175,
    16256097,
    3717818697,
    1287666208,
    1468685498,
    318920462,
    994590809,
    324843914,
    656077947,
    3894363637,
    2889785710,
    2583177105,
    255388168,
    603006969,
    1173768600,
    1441148311,
    102649491,
    557103324,
    370728986
]

for i in wrong:
    a, b = simon(i), game(i)
    if a != b:
        print(i, a, b)
'''
'''
s: 1
o: 2
s: 4
o: 8 -> 72

'''