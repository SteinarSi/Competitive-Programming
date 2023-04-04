from subprocess import run, PIPE
import os
import sys
'''
import os
from subprocess import Popen, PIPE

p = Popen('fortranExecutable', stdin=PIPE) #NOTE: no shell=True here
p.communicate(os.linesep.join(["input 1", "input 2"]))
'''

def test(program: str, inp: str, possible: bool):
    with open(inp, "r") as f:
        data = [int(line) for line in f.readlines()][1:]
    with open(inp, "r") as f:
        try:
            output = run("./" + program, stdin=f, stdout=PIPE).stdout.decode("utf-8").strip().split('\n')
            if len(output) == 1 and not possible and output[0] == "commence the thumb-twiddling":
                return "Correct answer"
            s = 0
            for line in output:
                s += data[int(line)-1]
            if s == sum(data)/2:
                return "Correct answer"
            else:
                return "Wrong answer"
        except:
            return "Runtime error"

def test_all(program):
    for file in os.listdir("possible"):
        out = test(program, "possible/" + file, True)
        if out != "Correct answer":
            return out
    for file in os.listdir("impossible"):
        out = test(program, "impossible/" + file, False)
        if out != "Correct answer":
            return out
    return "Correct answer"

print(test_all(sys.argv[1]))
