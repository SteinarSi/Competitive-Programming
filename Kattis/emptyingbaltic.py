from heapq import heappush, heappop

h, w = (int(i) for i in input().split())
grid = [[int(i) for i in input().split()] for _ in range(h)]
sy,sx = (int(i) for i in input().split())

flow = [[None] * w for _ in range(h)]
done = [[False] * w for _ in range(h)]

queue = [(grid[sy-1][sx-1],sy-1,sx-1)]

ret = 0

while len(queue):
    f, y, x = heappop(queue)
    if done[y][x]: continue
    done[y][x] = True
    ret -= f

    for vy, vx in ((y-1,x),(y-1,x+1),(y,x+1),(y+1,x+1),(y+1,x),(y+1,x-1),(y,x-1),(y-1,x-1)):
        if vx >= 0 and vy >= 0 and vy < h and vx < w and grid[vy][vx] < 0 and not done[vy][vx] and (flow[vy][vx] is None or flow[vy][vx] > max(f, grid[vy][vx])):
            flow[vy][vx] = max(f, grid[vy][vx])
            heappush(queue, (max(f, grid[vy][vx]), vy, vx))
        
print(ret)
