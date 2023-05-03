#include <vector>
#include <iostream>
#include <iomanip>
#include <string>
#include <queue>

using namespace std;

const vector<pair<int,int>> dirs = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};
char dir_to_char(int dx, int dy){
    if (dx == -1) return 'L';
    if (dx ==  1) return 'R';
    if (dy == -1) return 'U';
    if (dy ==  1) return 'D';
    return 'E';
}

void bfs(vector<string> &map, vector<vector<int>> &dist, vector<vector<pair<int,int>>> &parent, int ax, int ay){
    queue<pair<int,int>> q;
    q.push({ax, ay});
    dist[ay][ax] = 0;
    while ( ! q.empty()){
        auto [x, y] = q.front();
        q.pop();
        for (auto [dx, dy] : dirs){
            if (x+dx < 0 || y+dy < 0 || x+dx >= map[0].size() || y+dy >= map.size()) continue;
            if (map[y+dy][x+dx] != '#' && dist[y+dy][x+dx] == -1) {
                dist[y+dy][x+dx] = 1 + dist[y][x];
                q.push({x+dx, y+dy});
                parent[y+dy][x+dx] = {x, y};
                if (map[y+dy][x+dx] == 'B') return;
            }
        }
    }
}

void backtrack(int ax, int ay, int bx, int by, vector<vector<int>> &dist, vector<vector<pair<int,int>>> &parent){
    if (dist[by][bx] == -1){
        cout << "NO\n";
        return;
    }
    string ret;
    while (bx != ax || by != ay){
        auto [px, py] = parent[by][bx];
        ret += dir_to_char(bx-px, by-py);
        bx = px;
        by = py;
    }
    cout << "YES\n" << ret.size() << endl; 
    for (int i {ret.size()-1}; i >= 0; cout << ret[i--]);
    cout << '\n';
}

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    int n, m, ax, ay, bx, by;
    cin >> n >> m;
    vector<string> map(n);
    for (int i {0}; i < n; i++){
        cin >> map[i];
        for (int j {0}; j < m; j++){
            if (map[i][j] == 'A') { ax = j; ay = i; }
            else if (map[i][j] == 'B') { bx = j; by = i; }
        }
    }
    vector<vector<pair<int,int>>> parent(n, vector<pair<int,int>>(m, {-1,-1})); // E for excellent job
    vector<vector<int>> dist(n, vector<int>(m, -1));
    bfs(map, dist, parent, ax, ay);
    backtrack(ax, ay, bx, by, dist, parent);
}