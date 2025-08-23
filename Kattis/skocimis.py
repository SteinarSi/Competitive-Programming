a,b,c = (int(i) for i in input().split())
print(max(c-b, b-a) - 1)
