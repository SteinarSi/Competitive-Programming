from datetime import date

year = int(input())

day1 = date(year, 5, 1)
day1 = day1.replace(day=1 + 6 - day1.weekday() + 7)
day2 = date(year, 6, 1)
day2 = day2.replace(day=1 + 6 - day2.weekday() + 14)

print((day2 - day1).days // 7, "weeks")