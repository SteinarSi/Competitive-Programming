#include <iostream>
#include <vector>
#include <iomanip>
#include <cstdint>

using namespace std;
using u64 = uint64_t;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    u64 n, m, a;
    while (true) {
        cin >> n >> m;
        u64 ret {0};
        if (n == 0 && m == 0) break;
        vector<bool> xs(1000000000);
        for (u64 i {0}; i < n+m; i++) {
            cin >> a;
            ret += xs[a];
            xs[a] = true;
        }
        cout << ret << '\n';
    }
}
