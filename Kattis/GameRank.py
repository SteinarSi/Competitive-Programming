inp = input()
rank = 25
stars = 0
streak = 0
leg = False
for c in inp:
    if not leg:
        if c == "W":
            stars += 1
            streak += 1
            if rank <= 5:
                streak = 0
            if streak >=3:
                stars += 1
            if rank >= 21:
                if stars > 2:
                    rank -= 1
                    stars -= 2
            elif rank >= 16:
                if stars > 3:
                    rank -= 1
                    stars -= 3
            elif rank >= 11:
                if stars > 4:
                    rank -= 1
                    stars -= 4
            else:
                if stars > 5:
                    rank -= 1
                    stars -= 5
            if rank <= 0:
                leg = True

        else:
            streak = 0
            if rank == 20 and stars > 0:
                stars -= 1
            if rank < 20:
                stars -= 1
            if 20 > rank >= 15:
                if stars < 0:
                    rank += 1
                    stars = 2
            elif rank >= 10:
                if stars < 0:
                    rank += 1
                    stars = 3
            else:
                if stars < 0:
                    rank += 1
                    stars = 4

if leg:
    print("Legend")
else:
    print(rank)