#include <iostream>
#include <iomanip>
#include <vector>
#include <queue>
#include <cstdint>

using namespace std;
using i64 = int64_t;

vector<i64> dijkstra(vector<vector<pair<i64,i64>>> &graph) {
    vector<i64> dist(graph.size(),999999999);
    dist[0] = 0;
    priority_queue<pair<i64,i64>> queue;
    queue.push({0,0});

    while ( ! queue.empty()) {
        i64 u = queue.top().second;
        queue.pop();
        for (auto [v,w] : graph[u]) {
            i64 distv = dist[u] + w;
            if (distv < dist[v]) {
                dist[v] = distv;
                queue.push({-distv,v});
            }
        }
    }

    return dist;
}

i64 flowerize(vector<vector<pair<i64,i64>>> &graph, vector<i64> &dist) {
    i64 ret = 0;
    vector<bool> flowered(graph.size(),false);
    queue<int> queue;
    queue.push(graph.size()-1);

    while ( ! queue.empty()) {
        int u = queue.front();
        queue.pop();

        for (auto [v,w] : graph[u]) {
            if (dist[v] + w == dist[u]) {
                if ( ! flowered[v]) {
                    queue.push(v);
                    flowered[v] = true;
                }
                ret += 2 * w;
            }
        }
    }
    
    return ret;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    i64 p, t, u, v, w;
    cin >> p >> t;
    vector<vector<pair<i64,i64>>> graph(p);
    for (i64 i {t}; i --> 0;) {
        cin >> u >> v >> w;
        graph[u].push_back({v,w});
        graph[v].push_back({u,w});
    }

    vector<i64> dist = dijkstra(graph);
    cout << flowerize(graph, dist) << '\n';
}
