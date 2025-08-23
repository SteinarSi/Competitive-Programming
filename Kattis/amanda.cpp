#include <vector>
#include <iostream>
#include <algorithm>

using namespace std;
using i64 = int64_t;

i64 color(int u, bool should_color, vector<vector<int>> &graph, vector<int> &lounge, vector<int> &run){
    i64 ret {0};
    run.push_back(u);
    if (should_color){ 
        ret += 1;
        lounge[u] = 1;
    }
    else lounge[u] = 0;
    for (int v : graph[u]){
        if (lounge[v] == lounge[u]) return -9223372036854775806;
        if (lounge[v] != -1) continue;
        ret += color(v, ! should_color, graph, lounge, run);
    }
    return ret;
}


int main(){
    int n, m;
    cin >> n >> m;

    vector<int> lounge(n, -1);
    vector<pair<int, int>> maybes;

    int from, to, c;
    for (int i {0}; i < m; i++){
        cin >> from >> to >> c;
        from--;
        to--;
        if (c == 0){
            if (lounge[from] == 1 || lounge[to] == 1){
                cout << "impossible\n";
                return 0;
            }
            lounge[from] = 0;
            lounge[to] = 0;
        }
        else if (c == 2){
            if (lounge[from] == 0 || lounge[to] == 0){
                cout << "impossible\n";
                return 0;
            }
            lounge[from] = 1;
            lounge[to] = 1;
        }
        else{
            maybes.push_back({from, to});
        }
    }
    vector<vector<int>> maybe_graph(n);
    for (auto [from, to] : maybes){
        if (lounge[from] == 1 && lounge[to] == 1 || lounge[from] == 0 && lounge[to] == 0){
            cout << "impossible\n";
            return 0;
        }
        maybe_graph[from].push_back(to);
        maybe_graph[to].push_back(from);
    }

    vector<int> run;
    i64 ret {0};
    for (int u {0}; u < n; u++){
        if (lounge[u] == 1) ret += 1;
    }
    i64 score1, score2;
    for (int u {0}; u < n; u++){
        if (lounge[u] != -1) continue;
        run = {};
        score1 = color(u, true, maybe_graph, lounge, run);
        for (int v : run) lounge[v] = -1;
        run = {};
        score2 = color(u, false, maybe_graph, lounge, run);
        for (int v : run) lounge[v] = -1;
        if (score1 < 0){
            if (score2 < 0){
                cout << "impossible\n";
                return 0;
            }
            else ret += color(u, false, maybe_graph, lounge, run);
        }else{
            if (score2 < 0 || score1 < score2) ret += color(u, true, maybe_graph, lounge, run);
            else ret += color(u, false, maybe_graph, lounge, run);
        }
    }
    cout << ret << '\n';
}