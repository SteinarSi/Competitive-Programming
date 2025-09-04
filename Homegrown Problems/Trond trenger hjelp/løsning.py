from tests import test1, test2, test3, test4, test5
from data import data

def solve(data):
    n = len(data)
    opt = [0] * n
    opt[0] = data[0]
    opt[1] = max(data[0], data[1])

    for i in range(2, n):
        opt[i] = max(opt[i - 1], data[i] + opt[i - 2])

    return opt[-1]


# for test in test1, test2, test3, test4, test5:
#     print(len(test), solve(test))

# print(solve(data))

data1 = [15,2,8,11,4,13,1,9,6,14,3,10,5,12,0,7,15,2,8,11,4,13,1,9,6,14,3,10,5,12,0,7,15,2,8,11,4,13,1,9,6,14,3,10,5,12,0,7,15,2,8,11,4,13,1]
sol1  = [15,    11,  13,  9,  14,  10,  12,    15,    11,  13,  9,  14,  10,  12,    15,    11,  13,  9,  14,  10,  12,    15,    11,  13  ]

data2 = [12,4,8,11,6,13,12,5,9,6,11,3,10,9,12,8,6,8,3,8,6,4,3,2,9,6,11,8,10,9,12,5,6,7,2,8,11,7,13,5,9,8,12,5,9,9,12,5,7,13,8,8,11,5,11,10]
sol2  = [12,    11,     12,  9,  11,  10,  12,    8,  8,  4,    9,  11,  10,  12,  6,  2,  11,  13,  9,  12,  9,  12,  7,   8,  11,  11   ]
# sol2  = [12,    11,  13,     9,  11,  10,  12,  6,  3,  6,      9,  11,  10,  12,    7,    11,  13,  9,  12,  9,  12,  7,   8,  11,  11   ]

print(solve(data1))
print(sum(sol1))
print(solve(data2))
print(sum(sol2))