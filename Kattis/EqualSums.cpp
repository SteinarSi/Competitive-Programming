#include <iostream>
#include <iomanip>
#include <vector>
#include <cstdint>
#include <map>

using namespace std;
using i64 = int64_t;

void solve(int n, vector<i64> &xs) {
    map<i64,vector<int>> seen;

    for (int mask_a {1}; mask_a < (1 << n); mask_a++) {
        i64 summ = 0;
        for (int i {0}; i < n; i++) if (mask_a & (1 << i)) summ += xs[i];
        auto [it, placed] = seen.emplace(summ, vector<int>());
        if ( ! placed) {
            for (int mask_b : it->second) {
                if ( ! (mask_a & mask_b)) {
                    for (int i {0}; i < n; i++) if (mask_a & (1 << i)) cout << xs[i] << ' ';
                    cout << '\n';
                    for (int i {0}; i < n; i++) if (mask_b & (1 << i)) cout << xs[i] << ' ';
                    cout << '\n';
                    return;
                }
            }
        }
        it->second.push_back(mask_a);
    }

    cout << "Impossible\n";
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int cases, n;
    vector<i64> xs(20);
    cin >> cases;
    for (int c {1}; c <= cases; c++) {
        cout << "Case #" << c << ":\n";
        cin >> n;
        for (int i {0}; i < n; cin >> xs[i++]);
        solve(n, xs);
    }
}
