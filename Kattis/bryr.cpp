#include <iostream>
#include <vector>
#include <iomanip>
#include <queue>

using namespace std;
using u64 = uint64_t;

u64 dijkstra(vector<vector<pair<u64, u64>>> &graph, u64 n) {
    vector<u64> opt(n, 99999999999999);
    opt[0] = 0;
    priority_queue<pair<u64, u64>> q;
    q.push({0, 0});
    while ( ! q.empty()) {
        auto [_, u] = q.top();
        q.pop();
        for (auto [v, c] : graph[u]) {
            if (opt[u] + c < opt[v]) {
                opt[v] = opt[u] + c;
                q.push({-opt[v], v});
            }
        }
    }
    return opt[n-1];
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    u64 n, m, a, b, c;
    cin >> n >> m;

    vector<vector<pair<u64, u64>>> graph(n);
    for (int i {0}; i < m; i++){
        cin >> a >> b >> c;
        a--; b--;
        graph[a].push_back({b, c});
        graph[b].push_back({a, c});
    }

    cout << dijkstra(graph, n) << endl;
}
