from datetime import datetime

dd, _, mmm, yy = input().split()
print(datetime.strptime(f'{dd} {mmm[1:].title()} {yy}', '%d %b %y').strftime("%Y-%m-%d"))
