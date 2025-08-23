THREES = [3**i for i in range(20)]

def make(w,i):
    if w == 0: return ('', '')
    if i < 0: return None

    skip = make(w,i-1)
    if skip is not None:
        return skip
    if w < 0:
        left = make(w+THREES[i],i-1)
        if left is not None:
            l,r = left
            return (f'{THREES[i]} {l}', r)
    else:
        right = make(w-THREES[i],i-1)
        if right is not None:
            l,r = right
            return (l, f'{THREES[i]} {r}')

    return None

for c in range(int(input())):
    x = int(input())
    ret = make(x, 20)
    if c != 0:
        print()
    print(f'left pan: {ret[0]}\nright pan: {ret[1]}')
