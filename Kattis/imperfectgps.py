from math import sqrt

def length(path):
    ret = 0
    for i in range(len(path)-1):
        dx = abs(path[i+1][0] - path[i][0])
        dy = abs(path[i+1][1] - path[i][1])
        ret += sqrt(dx**2 + dy**2)
    return ret

n, t = (int(i) for i in input().split())

actual_path = []
gps_path = []

x,y,d = (int(i) for i in input().split())
actual_path.append((x,y,d))
gps_path.append((x,y,d))
curr = d
for _ in range(1,n):
    x,y,d = (int(i) for i in input().split())
    last_x,last_y,last_d = actual_path[-1]

    delta_time = d - last_d
    for curr in range(curr+t, d+1, t):
        ddd = (curr-d) / delta_time
        gps_x = x + (x-last_x) * ddd
        gps_y = y + (y-last_y) * ddd
        gps_path.append((gps_x, gps_y,curr))

    actual_path.append((x,y,d))

gps_path.append(actual_path[-1])
actual = length(actual_path)
gps = length(gps_path)
lost = 100 * (actual - gps) / actual

print(lost)