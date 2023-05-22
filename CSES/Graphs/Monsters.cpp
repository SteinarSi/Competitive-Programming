#include <iostream>
#include <string>
#include <vector>
#include <iomanip>
#include <set>
#include <algorithm>

using namespace std;

vector<pair<int, int>> directions {{1, 0}, {-1, 0}, {0, -1}, {0, 1}};

vector<vector<long>> bfs(vector<string> &graph, int h, int w, vector<pair<int, int>> generation){
    vector<vector<long>> ret(h, vector<long>(w, 10000000000));
    int time {0};
    for (auto [x, y] : generation){
        ret[y][x] = time;
    }
    while ( ! generation.empty()){
        time++;
        vector<pair<int, int>> next;
        for (auto [x, y] : generation){
            for (auto [dx, dy] : directions){
                if (dx+x >= 0 && dx+x < w && dy+y >= 0 && dy+y < h && graph[dy+y][dx+x] == '.' && ret[dy+y][dx+x] >= 10000000000){
                    ret[dy+y][dx+x] = time;
                    next.push_back({dx+x, dy+y});
                }
            }
        }
        generation = next;
    }
    return ret;
}

pair<int,int> dir(char c){
    switch (c){
        case 'R': return {-1, 0};
        case 'L': return {1, 0};
        case 'U': return {0, 1};
        case 'D': return {0, -1};
        default: cout << "could not find anything for char " << c << '\n';
    }
}

void backtrack(vector<vector<char>> &dirs, int goal_x, int goal_y, int start_x, int start_y){
    vector<char> ret;
    while (goal_x != start_x || goal_y != start_y){
        ret.push_back(dirs[goal_y][goal_x]);
        auto [nx, ny] = dir(dirs[goal_y][goal_x]);
        goal_x += nx;
        goal_y += ny;
    }
    cout << ret.size() << '\n';
    reverse(ret.begin(), ret.end());
    for (char c : ret) cout << c;
    cout << '\n';
}

void solve(vector<string> &graph, int h, int w){
    vector<pair<int, int>> monster;
    pair<int, int> player;
    for (int y {0}; y < h; y++){
        for (int x {0}; x < w; x++){
            if(graph[y][x] == 'M'){
                monster.push_back({x, y});
            }else if (graph[y][x] == 'A'){
                player = {x, y};
            }
        }
    }
    vector<pair<int, int>> generation = {player};
    vector<vector<long>> monster_graph = bfs(graph, h, w, monster);
    vector<vector<bool>> visited(h, vector(w, false));
    vector<vector<char>> dirs(h, vector<char>(w, 'S'));
    int time {0};
    while ( ! generation.empty()){
        time++;
        vector<pair<int, int>> next;
        for (auto [x, y] : generation){
            for (auto [dx, dy] : directions){
                if (dx+x < 0 || dx+x >= w || dy+y < 0 || dy+y >= h){
                    cout << "YES\n";
                    backtrack(dirs, x, y, player.first, player.second);
                    return;
                }
                if (graph[dy+y][dx+x] == '.' && ! visited[dy+y][dx+x] && time < monster_graph[dy+y][dx+x]){
                    next.push_back({dx+x, dy+y});
                    visited[dy+y][dx+x] = true;
                    if (dx == 1) dirs[dy+y][dx+x] = 'R';
                    else if (dx == -1) dirs[dy+y][dx+x] = 'L';
                    else if (dy == 1) dirs[dy+y][dx+x] = 'D';
                    else dirs[dy+y][dx+x] = 'U';
                }
            }
        }
        generation = next;
    }
    cout << "NO\n";
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;
    int h, w;
    cin >> h >> w;
    vector<string> graph(h);
    for (int i {0}; i < h; cin >> graph[i++]);
    solve(graph, h, w);
}