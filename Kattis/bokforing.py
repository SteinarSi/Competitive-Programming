
n, q = input().split()
n, q = int(n), int(q)


restarts = [0]
updated = [0 for _ in range(n)]
updates = [0 for _ in range(n)]
money = "0"
n_restarts = 0

for _ in range(q):
    inn = input().split()
    if inn[0] == "SET":
        i = int(inn[1])-1
        updates[i] = int(inn[2])
        updated[i] = n_restarts
    elif inn[0] == "RESTART":
        n_restarts += 1
        money = inn[1]
    else:
        i = int(inn[1])-1
        if updated[i] >= n_restarts:
            print(updates[i])
        else:
            print(money)