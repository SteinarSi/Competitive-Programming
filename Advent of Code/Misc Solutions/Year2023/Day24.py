import z3
import sys

# TODO this doesnt work, need to fix it later

def parse_hail(s: str):
    pos, dir = s.strip().replace(',','').split("@")
    dir = tuple(map(int,dir.split()))
    pos = tuple(map(int,pos.split()))
    return (pos, dir)

hails = list(map(parse_hail, sys.stdin.readlines()))

def to_bit_vec(field: str):
    # return z3.BitVec(field, 64)
    return z3.Real(field)

x,  y,  z =  to_bit_vec('x'),  to_bit_vec('y'),  to_bit_vec('z')
dx, dy, dz = to_bit_vec('dx'), to_bit_vec('dy'), to_bit_vec('dz')

solver = z3.Solver()

for i, ((a,b,c),(da,db,dc)) in enumerate(hails[:3]):
    t = to_bit_vec(f't_{i}')
    solver.add(t >= 0)
    solver.add(x + dx * t == a + da * t)
    solver.add(y + dy * t == b + db * t)
    solver.add(z + dz * t == c + dc * t)

assert solver.check() == z3.sat

m = solver.model()

def eval(field):
    return m.eval(field).as_long()

x, y, z = map(eval, [x, y, z])

print(x, y, z)
print(x+y+z)

'''
py src/Day24.py < inputs/year2023/day24-test.txt
py src/Day24.py < inputs/year2023/day24-input.txt
'''