n = int(input())
highest = 0
for _ in range(n):
    s = input()
    if s == '/':
        print(highest+10-highest%10)
    else:
        s = int(s)
        highest = max(highest,s)
        print(s)
