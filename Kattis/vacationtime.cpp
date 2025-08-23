#include <iostream>
#include <string>
#include <vector>
#include <queue>
#include <iomanip>
#include <cstdint>

using namespace std;
using i64 = int64_t;
const i64 INF = 999999999;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    i64 a,f,o,d,c;
    string m;

    cin >> a >> f;

    vector<vector<pair<i64,pair<i64,bool>>>> graph(a);
    while (f --> 0) {
        cin >> o >> d >> c >> m;
        graph[o].push_back({d,{c,m=="A380"}});
    }

    priority_queue<pair<bool,pair<i64,i64>>> queue;
    queue.push({false,{0,0}});

    vector<i64> dist(a*2,INF);
    dist[0] = 0;
    while ( ! queue.empty()) {
        auto [h,cu] = queue.top();
        queue.pop();
        auto [c,u] = cu;
        i64 ua = u + a * h;
        if (-c != dist[ua]) continue;

        for (auto [v, wa] : graph[u]) {
            auto [w,a380] = wa;
            i64 va = v + a * (h || a380);
            i64 distv = dist[ua] + w;
            if (distv < dist[va]) {
                dist[va] = distv;
                queue.push({h || a380, {-distv,v}});
            }
        }
    }

    if (dist[2*a-1] < INF) cout << dist[2*a-1] << '\n';
    else cout << "-1\n";
}
