from random import choice

guess = ""
for _ in range(int(input())):
    options = ['P','I','Z','A']
    while len(options) > 1:
        next = choice(options)
        print(guess + next, flush=True)
        match input():
            case "1":
                guess += next
                break
            case "2":
                exit()
            case _:
                options.remove(next)
    else:
        guess += options[0]
print(guess)
