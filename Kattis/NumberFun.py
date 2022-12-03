# Skrevet 25.11.19

runder = int(input())
for i in range(runder):
    kankanikke = False
    tall = input().split()
    if int(tall[0]) + int(tall[1]) == int(tall[2]):
        kankanikke = True
    elif int(tall[0]) - int(tall[1]) == (int(tall[2])) or int(tall[1]) - int(tall[0]) == int(tall[2]):
        kankanikke = True
    elif int(tall[0]) * int(tall[1]) == int(tall[2]):
        kankanikke = True
    elif int(tall[0]) / int(tall[1]) == int(tall[2]) or int(tall[1]) / int(tall[0]) == int(tall[2]):
        kankanikke = True
    if kankanikke:
        print("Possible")
    else:
        print("Impossible")