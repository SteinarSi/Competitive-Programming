# Skrevet 28.09.20

antall_sekvenser = int(input())

for i in range(antall_sekvenser):
    byer = []
    antall_stedsnavn = int(input())
    for j in range(antall_stedsnavn):
        by = input()
        if not by in byer:
            byer.append(by)
    print(len(byer))