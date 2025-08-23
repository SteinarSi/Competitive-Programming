#include <iostream>
#include <vector>
#include <iomanip>
#include <algorithm>

using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    i64 n;
    cin >> n;
    vector<i64> xs(n);
    for (int i {0}; i < n; cin >> xs[i++]);

    i64 m {xs[n-1]};
    i64 ret {0};
    for (int i {n-1}; i >= 0; i--) {
        ret += min(m, xs[i]);
        m = min(m, xs[i]);
    }
    cout << ret << '\n';
}
