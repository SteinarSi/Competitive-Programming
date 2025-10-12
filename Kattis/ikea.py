k = int(input())
n = int(input())

names = []
weights = []
for _ in range(n):
    name, weight = input().split()
    names.append(name)
    weights.append(int(weight))

indices = sorted(list(range(n)), key=lambda i: (weights[i],i))
    
fl = n // k
cl = (n+k-1) // k
first = sum((weights[i] for i in indices[:cl]))
second = sum((weights[i] for i in indices[cl:cl+fl]))

t = cl if first < second else fl

print(sum((weights[i] for i in indices[:t])))
for name in sorted(names[i] for i in indices[:t]):
    print(name)
