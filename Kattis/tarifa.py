#Skrevet 30.08.19

BpM = int(input())
antallmåneder = int(input())
bytetotalt = BpM * (antallmåneder + 1)
databrukt = 0
i = 0
while i < antallmåneder:
    databrukt += int(input())
    i += 1

print(bytetotalt - databrukt)