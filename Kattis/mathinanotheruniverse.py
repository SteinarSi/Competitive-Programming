from math import floor

def compute(xs):
    match (xs.rfind('/'),xs.rfind('*')):
        case (-1,-1):
            return eval(xs)
        case (-1,j):
            return compute(xs[:j]) * eval(xs[j+1:])
        case (i,-1):
            return compute(xs[:i]) / eval(xs[i+1:])
        case (i, j):
            if i < j:
                return compute(xs[:j]) * eval(xs[j+1:])
            else:
                return compute(xs[:i]) / eval(xs[i+1:])

print(floor(compute(input())))
