#include <iostream>
#include <vector>
#include <iomanip>
#include <set>
#include <cstdint>

using namespace std;
using u64 = uint64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    u64 n, m, i, a, b, u, v, w;
    cin >> n >> m;
    vector<multiset<u64>> graph(n+1);
    for (i = m; i --> 0;) {
        cin >> a >> b;
        graph[a].insert(b);
    }
    cin >> w >> u;
    for (i = 1; i <= w; i++) {
        cin >> v;
        auto it = graph[u].find(v);
        if (it == graph[u].end()) {
            cout << i << '\n';
            return 0;
        }
        graph[u].erase(it);
        u = v;
    }
    if (w == m) cout << "correct\n";
    else cout << "too short\n";
}
