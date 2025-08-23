#include <iostream>
#include <iomanip>
#include <vector>
#include <cstdint>
#include <queue>

using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    i64 a, h, n, m, u, v, ma, mh;
    cin >> a >> h >> n >> m;

    vector<vector<pair<i64,pair<i64,i64>>>> graph(n+1);
    for (;m --> 0;) {
        cin >> u >> v >> ma >> mh;
        graph[u].push_back({v, {ma,mh}});
    }

    vector<i64> best(n+1,0);
    best[1] = h;
    priority_queue<pair<i64,i64>> queue;
    queue.push({h,1});

    while ( ! queue.empty()) {
        auto [ph,u] = queue.top();
        queue.pop();
        
        if (best[u] > ph) continue;
        for (auto [v, mah] : graph[u]) {
            auto [ma, mh] = mah;
            i64 rem = ph - ma * ((mh+a-1) / a - 1);
            if (rem > best[v]) {
                best[v] = rem;
                queue.push({rem,v});
            }
        }
    }

    if (best[n]) cout << best[n] << '\n';
    else cout << "Oh no\n";
}
