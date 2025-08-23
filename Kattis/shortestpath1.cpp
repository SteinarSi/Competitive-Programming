#pragma GCC optimize("O3")

#include <iostream>
#include <iomanip>
#include <vector>
#include <queue>

using namespace std;
using i64 = int64_t;

void dijkstra(vector<vector<pair<i64, i64>>> &graph, vector<i64> &dist, i64 s) {
    priority_queue<pair<i64,i64>> q;
    q.push({0, s});

    while ( ! q.empty()) {
        auto [nd, u] = q.top();
        q.pop();
        if (dist[u] != -1) continue;
        dist[u] = -nd;
        for (auto [v, w] : graph[u]) {
            if (dist[v] == -1) {
                q.push({-(dist[u] + w), v});
            }
        }
    }
}

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    i64 n, m, q, s, u, v, w;
    while (cin >> n >> m >> q >> s && n > 0) {
        vector<vector<pair<i64, i64>>> graph(n);
        vector<i64> dist(n, -1);
        for (int i {0}; i < m; i++) {
            cin >> u >> v >> w;
            graph[u].push_back({v, w});
        }

        dijkstra(graph, dist, s);

        for (int i {0}; i < q; i++) {
            cin >> u;
            if (dist[u] != -1) cout << dist[u] << '\n';
            else cout << "Impossible\n";
        }
        cout << '\n';
    }
}
