#include <iostream>
#include <string>
#include <vector>
#include <iomanip>
#include <set>

using namespace std;

vector<string> build_graph(int h){
    vector<string> ret;
    string line;
    for (int i {0}; i < h; i++){
        cin >> line;
        ret.push_back(line);
    }
    return ret;
}   

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

void solve(vector<string> &graph, int h, int w){
    vector<pair<int, int>> fire;
    pair<int, int> player;
    for (int y {0}; y < h; y++){
        for (int x {0}; x < w; x++){
            if(graph[y][x] == '*'){
                fire.push_back({x, y});
            }else if (graph[y][x] == '@'){
                player = {x, y};
            }
        }
    }
    vector<pair<int, int>> generation = {player};
    vector<vector<long>> fire_graph = bfs(graph, h, w, fire);
    vector<vector<long>> player_graph = bfs(graph, h, w, generation);
    vector<vector<bool>> visited(h, vector(w, false));
    int time {0};
    while ( ! generation.empty()){
        time++;
        vector<pair<int, int>> next;
        for (auto [x, y] : generation){
            for (auto [dx, dy] : directions){
                if (dx+x < 0 || dx+x >= w || dy+y < 0 || dy+y >= h){
                    cout << time << '\n';
                    return;
                }
                if (graph[dy+y][dx+x] == '.' && ! visited[dy+y][dx+x] && player_graph[dy+y][dx+x] < fire_graph[dy+y][dx+x]){
                    next.push_back({dx+x, dy+y});
                    visited[dy+y][dx+x] = true;
                }
            }
        }
        generation = next;
    }
    cout << "IMPOSSIBLE\n";
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int n;
    cin >> n;
    vector<string> graph;
    int h;
    int w;
    for (int i {0}; i < n; i++){
        cin >> w >> h;
        graph = build_graph(h);
        solve(graph, h, w);
    }
}