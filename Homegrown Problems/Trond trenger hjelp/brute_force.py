from data import data


def brute_force(i):
    if i < 0:
        return 0
    if i == 0:
        return data[i]
    else:
        return max(brute_force(i - 1), brute_force(i - 2) + data[i])


print(brute_force(len(data) - 1))
