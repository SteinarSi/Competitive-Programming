#include <iostream>
#include <vector>
#include <iomanip>
#include <queue>

using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    i64 n, m, s, t, c, u, v, p;
    cin >> n >> m >> s >> t >> c;
    vector<vector<pair<i64,i64>>> graph(n+1);
    while (m --> 0) {
        cin >> u >> v >> p;
        graph[u].push_back({v,p});
        graph[v].push_back({u,p});
    }

    priority_queue<pair<i64,i64>> queue;
    vector<i64> best(n+1,0);
    best[s] = c;
    queue.push({c,s});
    while ( ! queue.empty()) {
        auto [c, u] = queue.top();
        queue.pop();
        if (best[u] > c) continue;
        for (auto [v, p] : graph[u]) {
            i64 c2 = c - (c * p + 99) / 100;
            if (c2 > best[v]) {
                best[v] = c2;
                queue.push({c2,v});
            }
        }
    }
    cout << best[t] << '\n';
}
