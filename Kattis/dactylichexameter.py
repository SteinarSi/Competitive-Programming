xs = input()

for _ in range(5):
    if xs.startswith("-uu"):
        xs = xs[3:]
    elif xs.startswith("--"):
        xs = xs[2:]
    else:
        print("no")
        exit()

if xs == "--" or xs == "-u":
    print("yes")
else:
    print("no")
