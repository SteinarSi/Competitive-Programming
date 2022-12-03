streng = input()
dicto = dict()
words = streng.split(" ")
for word in words:
    if word[0] in  dicto:
        dicto[word[0]] += 1
    else:
        dicto[word[0]] = 1
        
høyeste = 0
for i in dicto.keys():
    if dicto[i] > høyeste:
        høyeste = dicto[i]
print(høyeste)