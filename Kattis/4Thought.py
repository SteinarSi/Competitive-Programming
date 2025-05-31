memo = {}
ops = ('+', '-', '*', '//')
for op1 in ops:
    for op2 in ops:
        for op3 in ops:
            expr = f'4 {op1} 4 {op2} 4 {op3} 4'
            value = eval(expr)
            memo[value] = f'{expr.replace("//", "/")} = {value}'

for _ in range(int(input())):
    print(memo.get(int(input()), 'no solution'))
