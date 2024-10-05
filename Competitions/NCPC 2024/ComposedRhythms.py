n = int(input())

q = n // 3
r = n - 3*q

def threes(x):
    return " ".join(["3"] * x)

match r:
    case 0:
        print(q)
        print(threes(q))
    case 1:
        print(q+1)
        print(threes(q-1), 2, 2)
    case 2:
        print(q+1)
        print(threes(q), 2)
