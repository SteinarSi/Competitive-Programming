#include <vector>
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;
using u64 = uint64_t;

vector<pair<int,int>> DIRS {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};

u64 dfs(int ux, int uy, vector<vector<bool>> &visited, vector<string> &map){
    visited[uy][ux] = true;
    u64 ret {map[uy][ux] == 'G'};
    for (auto [dx, dy] : DIRS){
        if (map[uy+dy][ux+dx] == 'T') return ret;
    }
    for (auto [dx, dy] : DIRS){
        int x = ux + dx;
        int y = uy + dy;
        if ( ! visited[y][x] && map[y][x] != '#') ret += dfs(x, y, visited, map);
    }
    return ret;
}

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    u64 w, h;
    int px, py;
    int f = -1;
    cin >> w >> h;
    vector<string> map(h);
    for (int i {0}; i < h; i++){
        cin >> map[i];
        if (f == -1){
            f = map[i].find('P');
            if (f != -1){
                px = f;
                py = i;
            }
        }
    }
    vector<vector<bool>> visited(h, vector<bool>(w, false));
    cout << dfs(px, py, visited, map) << endl;
}