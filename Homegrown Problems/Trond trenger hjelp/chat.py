def maks_fortjeneste(kunder):
    if not kunder:
        return 0
    
    # dp[i] representerer maksimal fortjeneste fra posisjon i til slutten
    dp = [0] * (len(kunder) + 2)
    
    # Jobber bakover fra slutten
    for i in range(len(kunder)-1, -1, -1):
        # Maksimum av:
        # 1. Ta denne kunden (og hopp over neste) 
        # 2. Ikke ta denne kunden
        dp[i] = max(kunder[i] + dp[i+2], dp[i+1])
    
    return dp[0]

# Tronds liste
kunder = [12,4,8,11,6,13,12,5,9,6,11,3,10,9,12,8,6,8,3,8,6,4,3,2,9,6,11,8,10,9,12,5,6,7,2,8,11,7,13,5,9,8,12,5,9,9,12,5,7,13,8,8,11,5,11,10]
kunder = [12, 4, 8, 11 , 6, 13 , 12, 5, 9, 6, 11, 3, 10, 9, 12, 8, 6, 8, 3, 8, 6, 4, 3, 2, 9, 6, 11, 8, 10, 9, 12, 5, 6, 7, 2, 8, 11, 7, 13, 5, 9, 8, 12, 5, 9, 9, 12, 5, 7, 13, 8, 8, 11, 5, 11, 10]

resultat = maks_fortjeneste(kunder)
print(f"Maksimal fortjeneste: {resultat} millioner kroner")
