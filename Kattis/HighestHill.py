n = input()
highest = 0 
inputs = [int(m) for m in input().strip().split()]

prev = inputs[0]
start = inputs[0]
count = True

for i in range(1, len(inputs)):
    m = inputs[i]
    if count:
        if m < prev:
            count = False
            highest = max(highest, min(prev-start, prev-m))
        else:
            prev = m
    else:
        if m <= inputs[i-1]:
            highest = max(highest, min(prev-start, prev-m))
        else:
            prev = m
            count = True
            start = inputs[i-1]

highest = max(highest, min(prev-start, prev-m))
print(highest)