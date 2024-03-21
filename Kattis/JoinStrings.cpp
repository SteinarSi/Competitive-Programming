#include <iostream>
#include <vector>
#include <iomanip>
#include <cstdint>

using namespace std;
using i64 = int64_t;

void concat(vector<string> &xs, vector<vector<i64>> &follows, i64 u) {
    cout << xs[u];
    for (i64 v : follows[u]) concat(xs, follows, v);
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    
    i64 n, a, b, i;
    cin >> n;
    vector<string> xs(n);
    vector<vector<i64>> follows(n);
    vector<bool> starts(n, true);
    for (i = 0; i < n; cin >> xs[i++]);
    for (i = 0; i < n-1; i++) {
        cin >> a >> b;
        a--;
        b--;
        follows[a].push_back(b);
        starts[b] = false;
    }
    for (i = 0; i < n; i++) {
        if (starts[i]) {
            concat(xs, follows, i);
            break;
        }
    }
    cout << endl;
}
