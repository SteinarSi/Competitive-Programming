from math import log2, ceil

def left(i): return 2*i
def right(i): return 2*i+1
def parent(i): return i // 2
def index(T, i): return len(T) // 2 + i
def lookup(tree, i): return tree[index(tree, i)]

def update(idx, value, tree, op):
    idx = index(tree, idx)
    tree[idx] = value
    while (idx := parent(idx)) > 0:
        tree[idx] = op([tree[left(idx)], tree[right(idx)]])

def query_helper(tree, l, r):
    if l == r: 
        yield tree[l] # [l, r]
        return
    yield tree[l] # [l, r>
    yield tree[r] # [l, r]
    while True:
        pl, pr = parent(l), parent(r)
        if pl == pr: return
        if l % 2 == 0:                 # if left goes right, collect
            yield tree[right(pl)]
        if r % 2 == 1:                 # if right goes left, collect
            yield tree[left(pr)]
        l,r = pl, pr

def query(tree, l, r, op):
    return op(query_helper(tree, index(tree, l) , index(tree, r)))

n, k = (int(i) for i in input().split())
tree = [0] * 2**(ceil(log2(n))) * 2 #n*[0]

for _ in range(k):
    line = input().split()
    if line[0] == 'C':
        l, r = int(line[1])-1, int(line[2])-1
        print(query(tree, l, r, sum))
    elif line[0] == 'F':
        idx = int(line[1])-1
        update(idx, 1-lookup(tree, idx), tree, sum)