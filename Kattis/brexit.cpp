#include <iostream>
#include <vector>
#include <iomanip>
#include <queue>

using namespace std;
using u64 = uint64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    u64 c,p,x,l,a,b;

    cin >> c >> p >> l >> x;
    vector<u64> curr(c+1, 0);
    vector<u64> of(c+1, 0);
    vector<bool> out(c+1,false);
    vector<vector<u64>> graph(c+1);
    queue<int> queue;

    for (;p --> 0;) {
        cin >> a >> b;
        of[a]++;
        of[b]++;
        curr[a]++;
        curr[b]++;
        graph[a].push_back(b);
        graph[b].push_back(a);
    }

    out[x] = true;
    queue.push(x);

    while ( ! queue.empty()) {
        a = queue.front();
        queue.pop();
        for (u64 b : graph[a]) {
            if (out[b]) continue;
            if (--curr[b] <= of[b] / 2) {
                out[b] = true;
                queue.push(b);
            }
        }
        if (out[l]) break;
    }

    if (out[l]) cout << "leave\n";
    else cout << "stay\n";
}
