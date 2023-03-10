def double_vowels(s):
    sum = 0
    for i in range(len(s)-1):
        if s[i:i+2] in {'aa', 'ee', 'ii', 'oo', 'uu', 'yy'}:
            sum += 1
    return sum

while True:
    n = int(input())
    if n == 0:
        break
    words = [input() for _ in range(n)]
    print(max(words, key=double_vowels))

