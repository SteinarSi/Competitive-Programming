ret = [0, 1, 2, 3]

for _ in range(5):
    a = int(input())
    b = int(input())
    ret[a], ret[b] = ret[b], ret[a]

print(ret[1])
