DAYS = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']

def parse(s):
    day,time = s.split()
    hh,mm = time.split(':')
    return int(mm) + 60*int(hh) + 24*60*DAYS.index(day)

t1 = parse(input())
t2 = parse(input())
if t2 <= t1:
    t2 += 7*24*60
duration = t2 - t1

days, rest = divmod(duration,24*60)
hours, minutes = divmod(rest, 60)

ret = []
for dur,unit in zip((days,hours,minutes),('day','hour','minute')):
    if dur > 1:
        ret.append(f'{dur} {unit}s')
    elif dur > 0:
        ret.append(f'{dur} {unit}')

if len(ret) == 2:
    print(ret[0],'and',ret[1])
else:
    print(', '.join(ret))
