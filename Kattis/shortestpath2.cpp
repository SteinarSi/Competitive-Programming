#pragma GCC optimize("O3")

#include <iostream>
#include <iomanip>
#include <vector>
#include <queue>
#include <cstdint>

using namespace std;
using i64 = uint64_t;

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    i64 n, m, q, s, u, v, t, p, d;
    while (cin >> n >> m >> q >> s && n) {
        vector<vector<pair<pair<i64,i64>,pair<i64,i64>>>> graph(n);
        while (m --> 0) {
            cin >> u >> v >> t >> p >> d;
            graph[u].push_back({{v,d},{t,p}});
        }
        vector<i64> dist(n,-1);

        priority_queue<pair<i64,i64>> queue;
        dist[s] = 0;
        queue.push({0,s});

        while ( ! queue.empty()) {
            auto [nt,u] = queue.top();
            queue.pop();
            t = - nt;
            if (dist[u] < t) continue;
            for (auto [vd,tp] : graph[u]) {
                auto [v,d] = vd;
                auto [t0,p] = tp;
                i64 c = 999999999999;
                if (t <= t0) {
                    c = d + t0;
                }
                else if (p > 0) {
                    c = d + t0 + p * ((t-t0+p-1) / p);
                }
                if (c < dist[v]) {
                    dist[v] = c;
                    queue.push({-c,v});
                }
            }
        }
        while (q-->0) {
            cin >> u;
            if (dist[u] == -1) cout << "Impossible\n";
            else cout << dist[u] << '\n';
        }
        cout << '\n';
    }
}
