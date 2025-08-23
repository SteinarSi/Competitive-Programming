import fileinput

for line in fileinput.input():
    print(f"{eval(line):.2f}")
