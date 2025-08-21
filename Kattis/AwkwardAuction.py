from functools import cache

def attempt(lo,hi,mi):
    return max(
        mi,
        0 if mi == lo else mi + bid(lo,mi-1),
        0 if mi == hi else b + bid(mi+1,hi)
    )

@cache
def bid(lo,hi):
    if lo >= hi:
        return lo

    mini = lo
    maxi = hi

    best = 99999999999999

    while maxi-mini > 6:
        medi1 = mini + (maxi-mini) // 3
        medi2 = maxi - (maxi-mini) // 3
        medi1_val = attempt(lo,hi,medi1)
        medi2_val = attempt(lo,hi,medi2)

        if medi1_val < medi2_val:
            best = min(best,medi1_val)
            maxi = medi2-1
        elif medi1_val > medi2_val:
            best = min(best,medi2_val)
            mini = medi1+1
        else:
            best = min(best,medi1_val)
            mini += 1

    return min(best,*(attempt(lo,hi,mi) for mi in range(mini,maxi+1)))

n, b = (int(i) for i in input().split())
print(bid(1,n))
