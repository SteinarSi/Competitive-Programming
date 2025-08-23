for x in range(1,1000):
    xs = x*x
    for y in range(x+1,1000):
        if x + y >= 1000:
            break
        z = 1000 - x - y
        if xs + y*y == z*z:
            print(x*y*z)
            exit()
