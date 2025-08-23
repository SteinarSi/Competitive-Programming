lo = 1
ma = 1
hi = 1
while True:
    print(f'buf[{ma}]', flush=True)
    response = input().strip()
    if response == '0':
        hi = ma - 1
        break
    elif response == "Segmentation fault (core dumped)".strip():
        break # fail
    elif response == "Too many reads".strip():
        break # fail
    else:
        lo = ma+1
        ma *= 2

guess = 1

while lo <= hi:
    guess = (lo + hi) // 2
    print(f'buf[{guess}]', flush=True)
    response = input().strip()
    if response == '0':
        if lo == hi:
            guess = guess - 1
        hi = guess-1
    elif response == "Segmentation fault (core dumped)".strip():
        break # fail
    elif response == "Too many reads".strip():
        break # fail
    else:
        lo = guess + 1
else:
    print(f'strlen(buf) = {guess+1}', flush=True)