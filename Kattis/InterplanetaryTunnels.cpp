#include <iostream>
#include <iomanip>
#include <vector>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int n, m, s, t, a, b;
    cin >> n >> m >> s >> t;

    vector<vector<int>> graph(n);

    for (int i {0}; i < m; i++) {
        cin >> a >> b;
        graph[a].push_back(b);
        graph[b].push_back(a);
    }

    vector<int> dist(n, 999999999);
    dist[s] = 0;
    vector<int> queue(n);
    queue[0] = s;
    int head {0};
    int tail {1};

    while (head < tail) {
        int u = queue[head++];
        for (int v : graph[u]) {
            if (dist[u] + 1 < dist[v]) {
                queue[tail++] = v;
                dist[v] = dist[u] + 1;
            }
        }
    }
    cout << (dist[t] + 1) / 2 << '\n';
}
