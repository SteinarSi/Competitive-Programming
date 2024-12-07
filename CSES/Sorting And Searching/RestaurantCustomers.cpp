#pragma GCC optimize("O3")

#include <iostream>
#include <vector>
#include <iomanip>
#include <algorithm>
#include <cstdint>

using namespace std;
using i64 = int64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    i64 n, a, b;
    cin >> n;
    vector<pair<i64,bool>> xs(2*n);
    for (i64 i {0}; i < 2*n; i += 2) {
        cin >> a >> b;
        xs[i] = {a, true};
        xs[i+1] = {b, false};
    }
    sort(xs.begin(), xs.end());
    i64 curr = 0;
    i64 best = 0;
    for (i64 i {0}; i < 2*n; i++) {
        if (xs[i].second) curr++;
        else curr--;
        best = max(best, curr);
    }
    cout << best << '\n';
}
