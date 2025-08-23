#include <vector>
#include <iostream>
#include <iomanip>
#include <cstdint>
#include <algorithm>

using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    i64 n,q,b;
    cin >> n >> q;
    vector<i64> xs(n+1);
    for (i64 i {1}; i <= n; cin >> xs[i++]);

    sort(xs.begin(), xs.end());
    for (i64 i {1}; i <= n; i++) {
        xs[i] += xs[i-1];
    }
    for (i64 i {0}; i < q; i++) {
        cin >> b;
        cout << xs[b] << '\n';
    }
}
