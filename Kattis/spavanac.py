hrs, mins = input().split()
hrs, mins = int(hrs), int(mins)
mins = (mins - 45) % 60
if mins >= 15:
    hrs = (hrs - 1)  % 24
print(hrs, mins)