from data import data

"""
For hver `i` fra 0 til `n` finner vi det optimale svaret på sublisten fra 0 til `i`, og lagrer det i listen `opt`. Vi bruker det til å finne de neste svarene.

Til å begynne med regner vi ut svaret for de to korteste sublistene:
* `i = 0`: vi tar det eneste tallet vi kan, `data[i]`.
* `i = 1`: vi tar det største av de to første tallene, `max(data[0], data[1])`.

Nå må vi finne svaret for listen opp til en vilkårlig `i`. Vi kan enten bruke det nye tallet `data[i]`, eller la være. 
* Om vi ignorerer det nye tallet, er den optimale løsningen det samme som for `i-1`, altså `opt[i-1]`.
* Om vi bruker det nye tallet er den optimale løsningen lik `data[i]` pluss den beste løsningen opp til `i`-1 som ikke bruker `data[i-1]`, det vil si `opt[i-2]`.

Og det er hele algoritmen!
"""

data = [12, 4, 8, 11 , 6, 13 , 12, 5, 9, 6, 11, 3, 10, 9, 12, 8, 6, 8, 3, 8, 6, 4, 3, 2, 9, 6, 11, 8, 10, 9, 12, 5, 6, 7, 2, 8, 11, 7, 13, 5, 9, 8, 12, 5, 9, 9, 12, 5, 7, 13, 8, 8, 11, 5, 11, 10]

def solve(data):
    n = len(data)
    opt = [0] * n
    opt[0] = data[0]
    opt[1] = max(data[0], data[1])

    for i in range(2, n):
        opt[i] = max(
            opt[i-1], 
            opt[i-2] + data[i]
        )

    return opt[-1]

print(solve(data))
