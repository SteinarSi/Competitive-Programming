def unrotate(msg, amount):
    return ((msg << amount) | (msg >> (20 - amount))) & 0xFFFFF


def untransform(t):
    b = t & 0xFFFFF
    a = t >> 20

    a = a ^ b

    b = unrotate(b, 6)
    b = b ^ 0xBF83F
    b = b ^ a

    a = a ^ 0xC21F3
    a = unrotate(a, 6)

    return (b << 20) | a


print(untransform(int(input())))
