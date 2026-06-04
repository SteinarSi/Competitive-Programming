import sys

for line in sys.stdin.readlines():
    line = line.strip()
    if '#' in line: break
    ones = line.count('1')
    if line.endswith('o') != (ones & 1): print(line[:-1] + '1')
    else: print(line[:-1] + '0')
