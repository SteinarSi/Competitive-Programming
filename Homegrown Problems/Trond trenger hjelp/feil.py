# data = [5, 7, 1, 1, 2, 6, 7, 5, 4, 2, 5, 1, 0, 1, 4, 6, 2, 1, 1, 1, 0, 10, 9] * 2
from data import data

# from tests import test1, test2, test3, test4, test5


def every_other(test):
    partall = 0
    oddetall = 0

    for i in range(len(test)):
        if i % 2 == 0:
            partall += test[i]
        else:
            oddetall += test[i]
    return max(partall, oddetall)


# for test in test1, test2, test3, test4, test5:
#     print(every_other(test))

print(every_other(data))
