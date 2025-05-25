#include <iostream>
#include <iomanip>
#include <vector>
#include <cstdint>
#include <queue>
#include <numeric>

using namespace std;
using u64 = uint64_t;

void bfs(vector<vector<u64>> &graph, u64 s, vector<u64> &dist) {
    dist[s] = 0;
    queue<u64> fifo;
    fifo.push(s);

    while ( ! fifo.empty()) {
        u64 u = fifo.front();
        fifo.pop();
        for (u64 v : graph[u]) {
            if (v != s && dist[v] == 0) {
                dist[v] = dist[u] + 1;
                fifo.push(v);
            }
        }
    }
}

u64 expected_dist(vector<u64> &holes, vector<u64> &dist_s, vector<u64> &dist_t, u64 p) {
    u64 ret = 0;
    for (u64 h : holes) {
        if (h == p) continue;
        ret += dist_s[p] + dist_t[h];
    }
    return ret;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    u64 n, m, k, a, b;
    cin >> n >> m >> k;
    vector<u64> holes(k);
    for (u64 i {0}; i < k; cin >> holes[i++]);
    vector<vector<u64>> graph(n+1);
    for (u64 i {0}; i < m; i++) {
        cin >> a >> b;
        graph[a].push_back(b);
        graph[b].push_back(a);
    }

    vector<u64> dist_s = vector<u64>(n+1,0);
    vector<u64> dist_t = vector<u64>(n+1,0);
    bfs(graph, 1, dist_s);
    bfs(graph, n, dist_t);

    u64 total_hole_dist = 0;
    for (u64 h : holes) {
        total_hole_dist += dist_t[h];
    }
    u64 best = dist_s[n] * (k-1);
    for (u64 p : holes) {
        best = min(best, (k-1) * dist_s[p] + total_hole_dist - dist_t[p]);
    }

    u64 d = gcd(best, k-1);
    cout << best / d << '/' << (k-1)/d << '\n';
}
