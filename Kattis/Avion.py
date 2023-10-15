caught = False
for i in range(1, 6):
    inn = input()
    if 'FBI' in inn:
        caught = True
        print(i, end=' ')
if not caught:
    print('HE GOT AWAY!')