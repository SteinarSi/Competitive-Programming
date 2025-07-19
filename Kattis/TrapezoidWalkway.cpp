#include <iostream>
#include <queue>
#include <vector>
#include <iomanip>
#include <cstdint>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n, a, b, h, s, t;
    double c;
    while (cin >> n && n) {
        vector<vector<pair<int,double>>> graph(1001);
        for (; n --> 0; ) {
            cin >> a >> b >> h;
            double c = 0.01 * (double) ((a + b) * h);
            graph[a].push_back({b, c});
            graph[b].push_back({a, c});
        }
        cin >> s >> t;
        vector<double> dist(1001, 999999999);
        dist[s] = 0;
        priority_queue<pair<double,int>> queue;
        queue.push({0,s});

        while ( ! queue.empty()) {
            auto [distu, u] = queue.top();
            queue.pop();
            if (-distu != dist[u]) continue;
            if (u == t) {
                cout << dist[u] << '\n';
                break;
            }
            
            for (auto [v, w] : graph[u]) {
                double distv = dist[u] + w;
                if (distv < dist[v]) {
                    dist[v] = distv;
                    queue.push({-distv,v});
                }
            }
        }
    }
}
