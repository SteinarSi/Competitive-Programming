import sys
recorded_scores = int(sys.stdin.readline())

scores = [input().strip().split('-') for _ in range(recorded_scores)]

def serve(a, b):
    return (a+b) % 4 in [0, 3]

finihshed = False
if int(scores[0][0]) == 11 and int(scores[0][1]) == 11:
    print("error 1")
    sys.exit() 
if int(scores[0][0]) == 11 or int(scores[0][1]) == 11:
    finihshed = True


for round in range(1, len(scores)):



    #Extracting the scores
    prevScore = scores[round - 1]
    currentScore = scores[round]

    if prevScore == currentScore:
        continue

    #values
    prevX = int(prevScore[0])
    prevY = int(prevScore[1])
    curX = int(currentScore[0])
    curY = int(currentScore[1])

    if curX == 11 and curY == 11:
        print("error", round + 1)
        sys.exit()        

    if serve(curX, curY) == serve(prevX, prevY):
        if not (curX >= prevX and curY >= prevY):
            print("error", round + 1)
            sys.exit()
    else:
        if not (curX >= prevY and curY >= prevX):
            print("error", round + 1)
            sys.exit()

    if finihshed:
        print("error", round + 1)
        sys.exit() 

    if curX == 11 or curY == 11:
        finihshed = True

print("ok" )