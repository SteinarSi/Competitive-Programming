m = len(input().split()) - 1
n = int(input())
people = [input() for _ in range(n)]
next = 0
a = []
b = []

while len(people) != 0:
    next = (next + m) % len(people)
    a.append(people[next])
    people.pop(next)
    a,b = b,a

if n & 1:
    a,b = b,a

print(len(a))
for name in a:
    print(name)
print(len(b))
for name in b:
    print(name)
