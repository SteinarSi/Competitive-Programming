def right(a,b,c):
    return a**2 + b**2 == c**2    

while True:
    a,b,c = (int(i) for i in input().split())
    if (a,b,c) == (0,0,0):
        break
    if right(a,b,c) or right(a,c,b) or right(b,a,c) or right(b,c,a) or right(c,a,b) or right(c,b,a):
        print("right")
    else:
        print("wrong")
