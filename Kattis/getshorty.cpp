#include <iostream>
#include <iomanip>
#include <queue>
#include <vector>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(4) << fixed;

    int n, m, u, v;
    double f;
    while (cin >> n >> m && n) {
        vector<vector<pair<int,double>>> graph(n);
        for (int i {0}; i < m; i++) {
            cin >> u >> v >> f;
            graph[u].push_back({v,f});
            graph[v].push_back({u,f});
        }

        vector<double> size(n,0);
        priority_queue<pair<double,int>> queue;
        queue.push({1.0,0});

        while ( ! queue.empty()) {
            auto [s, u] = queue.top();
            queue.pop();
            for (auto [v, f] : graph[u]) {
                if (s*f > size[v]) {
                    size[v] = s*f;
                    queue.push({s*f,v});
                }
            }
        }

        cout << size[n-1] << '\n';
    }
}
