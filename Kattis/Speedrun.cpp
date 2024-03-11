#include <iostream>
#include <vector>
#include <iomanip>
#include <algorithm>
#include <cstdint>

using namespace std;
using u64 = uint64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    u64 g, n, start, end;

    cin >> g >> n;
    vector<pair<u64,u64>> xs(n);

    for (u64 i {0}; i < n; i++) {
        cin >> start >> end;
        xs[i] = {end, start};
    }

    sort(xs.begin(), xs.end());

    u64 prev {0};
    u64 ret {0};

    for (auto [end, start] : xs) {
        if (start >= prev) {
            prev = end;
            ret++;
        }
    }

    if (ret >= g) cout << "YES\n";
    else          cout << "NO\n";
}
