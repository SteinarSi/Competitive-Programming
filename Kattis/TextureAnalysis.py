i = 1
while True:
    inn = input()
    if inn == "END":
        break
    if len(set(inn[1:-1].split("*"))) == 1:
        print(i, "EVEN")
    else:
        print(i, "NOT EVEN")
    i += 1
