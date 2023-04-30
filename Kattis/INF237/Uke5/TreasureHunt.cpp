#include <iostream>
#include <string>
#include <iomanip>
#include <vector>
#include <queue>
#include <algorithm>

using namespace std;

vector<pair<int,int>> dirs = {{0, 1}, {0, -1}, {-1, 0}, {1, 0}};
int inf = 99999999;

int dijkstra(const int N, const int M, const int K, const int SY, const int SX, const int GY, const int GX, vector<string> &map){
    vector<vector<vector<int>>> dist = vector<vector<vector<int>>>(N, vector<vector<int>>(M, vector<int>(K+1, inf)));
    dist[SY][SX][K] = 1;
    // weight, ((x, y), k)
    priority_queue<pair<int, pair<pair<int,int>, int>>, vector<pair<int, pair<pair<int,int>, int>>>, greater<pair<int, pair<pair<int,int>, int>>>> q;
    q.push({1, {{SX, SY}, K}});

    int xx, yy, req;
    while ( ! q.empty()){
        auto [d, p] = q.top();
        auto [pos, sk] = p;
        auto [x, y] = pos;
        q.pop();
        for (auto [dx, dy] : dirs){
            xx = x + dx;
            yy = y + dy;
            if (yy < 0 || yy >= N || xx < 0 || xx >= M) continue;

            if (map[yy][xx] == '.') req = 1;
            else if (map[yy][xx] == 'G') req = 1;
            else if (map[yy][xx] == 'F') req = 2;
            else if (map[yy][xx] == 'M') req = 3;
            else if (map[yy][xx] == '#') req = inf;
            else if (map[yy][xx] == 'S') req = inf;

            if (req > sk) continue;

            if (d < dist[yy][xx][sk-req]){
                dist[yy][xx][sk-req] = d;
                q.push({d, {{xx, yy}, sk-req}});
            }
        }
        if (sk < K && d+1 < dist[y][x][K]){

            dist[y][x][K] = d+1;
            q.push({d+1, {{x, y}, K}});
        }
    }
    int best = inf;
    for (int k {0}; k <= K; k++) best = min(best, dist[GY][GX][k]);

    return best!=inf ? best : -1;
}

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    int n, m, k;
    cin >> n >> m >> k;
    vector<string> map(n);
    pair<int, int> s, g;
    for (int i {0}; i < n; i++){
        cin >> map[i];
        for (int j {0}; j < m; j++){
            if (map[i][j] == 'S') s = {i, j};
            else if (map[i][j] == 'G') g = {i, j};
        }
    } 
    cout << dijkstra(n, m, k, s.first, s.second, g.first, g.second, map) << endl;
}