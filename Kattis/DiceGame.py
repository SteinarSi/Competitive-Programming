G_low_1,G_high_1,G_low_2,G_high_2 = [ int(x) for x in (input().split())]

E_low_1,E_high_1,E_low_2,E_high_2 = [ int(x) for x in (input().split())]

Gunnars_probability = sum([G_low_1,G_high_1,G_low_2,G_high_2])
Emma_probability = sum([E_low_1,E_high_1,E_low_2,E_high_2])
if Gunnars_probability < Emma_probability:
    print("Emma")
elif Emma_probability < Gunnars_probability:
    print("Gunnar") 
else : print("Tie") 