#include <vector>
#include <cmath>
#include <iostream>
#include <algorithm>
#include <iomanip>
#include <queue>

using namespace std;

vector<int> dijkstra(vector<vector<pair<int,int>>> &graph, int n, int start) {
    vector<int> ret(n, 999999999);
    vector<bool> done(n, false);
    ret[start] = 0;

    priority_queue<pair<int, int>> queue;
    queue.push({0,start});

    while ( ! queue.empty()) {
        auto [_, u] = queue.top();
        queue.pop();
        if (done[u]) continue;
        done[u] = true;
        for (auto [v, w] : graph[u]) {
            if (ret[u] + w < ret[v]) {
                ret[v] = ret[u] + w;
                queue.push({-ret[v],v});
            }
        }
    }

    return ret;
}

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int n, m, u, v, a, b;
    cin >> n >> m;
    vector<int> lodgings(n);
    for (int i {0}; i < n; cin >> lodgings[i++]);

    vector<vector<pair<int,int>>> alfur_graph(n);
    vector<vector<pair<int,int>>> benedikt_graph(n);

    for (; m --> 0 ;) {
        cin >> u >> v >> a >> b;
        u--;
        v--;
        alfur_graph[u].push_back({v, a});
        alfur_graph[v].push_back({u, a});
        benedikt_graph[u].push_back({v, b});
        benedikt_graph[v].push_back({u, b});
    }

    vector<int> alfur_dists = dijkstra(alfur_graph, n, 0);
    vector<int> benedikt_dists = dijkstra(benedikt_graph, n, n-1);

    int best = 999999999;
    for (int i {0}; i < n; i++) {
        best = min(best, alfur_dists[i] + benedikt_dists[i] + lodgings[i]);
    }
    cout << best << '\n';
}
