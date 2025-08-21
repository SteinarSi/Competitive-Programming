#include <iostream>
#include <iomanip>
#include <vector>
#include <queue>

using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    i64 n, m, t, u, v;
    double l, s1, s2;

    cin >> n >> m >> t;
    vector<vector<pair<pair<i64,double>,pair<double,double>>>> graph(n+1);

    while (m --> 0) {
        cin >> u >> v >> l >> s1 >> s2;
        graph[u].push_back({{v,l},{s1,s2}});
        graph[v].push_back({{u,l},{s1,s2}});
    }

    vector<double> time(n+1,-1);
    time[1] = 0;
    priority_queue<pair<double,i64>> queue;
    queue.push({0,1});

    while ( ! queue.empty()) {
        auto [du,u] = queue.top();
        queue.pop();
        du = -du;
        if (time[u] < du) continue;
        
        for (auto [vl, s] : graph[u]) {
            auto [v, l] = vl;
            auto [s1,s2] = s;

            double dv;
            if (du >= t) {
                dv = du + l / s2;
            }
            else if (du + l / s1 > t) {
                dv = t + (l - s1 * (t-du)) / s2;
            }
            else {
                dv = du + l / s1;
            }
            if (time[v] == -1 || dv < time[v]) {
                time[v] = dv;
                queue.push({-dv,v});
            }
        }
    }

    cout << time[n] << '\n';
}
