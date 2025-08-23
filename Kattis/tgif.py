from datetime import datetime

date = input()
day = ["MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"].index(input())

def friday(y):
    return ((datetime.strptime(date + " " + y, "%d %b %Y").timetuple().tm_yday - 1 + day) % 7) == 4

friday2 = friday("2024")
friday1 = friday2 if date == "29 FEB" else friday("2021")

if friday1 and friday2:
    print("TGIF")
elif friday1 or friday2:
    print("not sure")
else:
    print(":(")
