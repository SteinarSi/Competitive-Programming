timestamps = []

n = int(input())
for _ in range(n):
    xs = [int(i) for i in input().split()]
    for i in range(1, len(xs), 2):
        timestamps.append((xs[i],1))
        timestamps.append((xs[i+1]+1,-1))

timestamps.sort()

seconds = [0] * (n+1)
best_people = 0
curr_people = 0
prev_point = 0

for p, b in timestamps:
    seconds[curr_people] += p - prev_point
    prev_point = p
    curr_people += b
    best_people = max(best_people, curr_people)

print(best_people)
print(seconds[best_people])
